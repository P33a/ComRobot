# What is ComRobot
With ComRobot you can connect your PC to a robot or other device using the serial port or UDP packets.

It can diplay a real time chart of the received data an can also log those values to a file.

# Data format
It expects ASCII data using the format:

[*command*][*sep*][*data*][*end*]

where:

the *command* must start with a letter

*sep* can be a space or  a ':'

*data* can be any sequence of characters except the ones reserved to mark the *end*

*end* cam be a ';', a Line Feed (LF) or a Carrier Return (CR)

Some examples:

dte 100003; led 100; temp 0; dbg 29; loop 391; 

# Configuring ComRobot

ComRobot can be configured to work with the messages that you want to exchange with your robot.

## Displaying the received values
Each cell of the grid can be configured to display the value associated with a comand by filling its 
read field.

The recived values are updated when a special command is received. 
The default for that command is loop.

## Sending commands
The grid can be also configured to send commands. 

The Write field sets the command to be sent.

The current text is the data that is sent.

The trigger cell must be set in the fields Row and Col and that cell must be non zero to enable the sending of the command.

When the loop command is received alll comands with the trigger cell non zero will be sent.

## Adding buttons
A grid cell can be turned into a button by adding text between square brackets.
If we write [OK] in the cell, it will dislpay a button woth the caption 'OK'.

Whe we press the button it will change the value of that cell to 1 and it can be used to trigger the sending of a command.
After the command is sent the cell will have its value retuned to zero.

## Display values in the chart
We can set any receiving cell to be charted by selecting the checkbox charted.

Whe the char is refresshed a new line series will be added with the values for that cell.

