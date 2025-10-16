# ForthMSX - a Forth 2012 standard system for MSX

The goal of this ongoing project is to implement a "wicked fast" and complete Forth 2012 standard system for MSX.

This will also include a complete manual, similar to the manual I wrote for [Forth500](https://github.com/Robert-van-Engelen/Forth500).

If you don't have an MSX machine or just want to try out ForthMSX, then visit [WebMSX](http://webmsx.org). To run ForthMSX in WebMSX click on the WebMSX floppy drive icon on the bottom-left, then "Import Files to Disk" and select the `forth.bin` file located in your local copy of this project.

![ForthMSX](img/forthmsx1.png)

To run it in WebMSX:

   clear 100,&h8400
   bload "forth.bin",r

Type `80 width` and ENTER to switch to 80 characters per line. Type `words` and ENTER to list all Forth words, one screenful at a time (press a key to continue).

![ForthMSX](img/forthmsx2.png)

The screen editor in Forth is the same as BASIC. So you can use the same keys and ways to enter Forth commands, edit them, and redo them as desired.
