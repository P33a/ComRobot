/* Copyright (c) 2024  Paulo Costa
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in
     the documentation and/or other materials provided with the
     distribution.
   * Neither the name of the copyright holders nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE. */

#include <Arduino.h>


//#include <RPi_Pico_TimerInterrupt.h>

// Select the timer you're using, from ITimer0(0)-ITimer3(3)
// Init RPI_PICO_Timer
// RPI_PICO_Timer ITimer1(1);


int debug;
int LED_PWM;
float temperature;

uint32_t interval, last_cycle;
uint32_t loop_micros;

void set_interval(float new_interval)
{
  interval = new_interval * 1000000L;   // In microseconds
}

// Remote commands

#include "gchannels.h"

gchannels_t serial_commands;
commands_list_t pars_list;


void process_command(command_frame_t frame)
{
  pars_list.process_read_command(frame);


  if (frame.command_is("dt")) { 
     set_interval(frame.value);

  } else if (frame.command_is("led")) { 
     LED_PWM = frame.value;
     if (LED_PWM > 255) LED_PWM = 255;
     if (LED_PWM < 0) LED_PWM = 0;

  } // Put here more commands...
}



void serial_write(const char *buffer, size_t size)
{
  Serial.write(buffer, size);
}



void setup() 
{
  // Set the pins as input or output as needed
  pinMode(LED_BUILTIN, OUTPUT);

  
  serial_commands.init(process_command, serial_write);

  // Start the serial port with 115200 baudrate
  Serial.begin(115200);
  Serial.printf("Serial Begin");  

  float control_interval = 0.04;  // In seconds
  
  set_interval(control_interval);    // In seconds
  Serial.printf("End Setup()");  
}

void loop() 
{
  uint8_t b;
  if (Serial.available()) {  // Only do this if there is serial data to be read
  
    b = Serial.read();    
    serial_commands.process_char(b);
    //Serial.write(b);
  }  

  // Do this only every "interval" microseconds 
  uint32_t now = micros();
  uint32_t delta = now - last_cycle; 
  if (delta >= interval) {
    loop_micros = micros();
    last_cycle = now;


    // Read and process sensors
    temperature = analogReadTemp(); // Get internal temperature
     
    // Control things here

    // Calc outputs
    analogWrite(LED_BUILTIN, LED_PWM);
    
    // Debug information
    serial_commands.send_command("dte", delta);

    serial_commands.send_command("led", LED_PWM);
    serial_commands.send_command("temp", temperature);
    
    Serial.print(" cmd: ");
    Serial.print(serial_commands.frame.command);
    Serial.print("; ");
      
    debug = serial_commands.out_count;
    serial_commands.send_command("dbg", debug); 
    serial_commands.send_command("loop", micros() - loop_micros);  
     
    serial_commands.flush();   
    Serial.println();
  }
    
}

