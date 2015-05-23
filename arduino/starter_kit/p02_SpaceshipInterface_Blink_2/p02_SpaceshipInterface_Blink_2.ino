/*
  Arduino Starter Kit example
 Project 2  - Spaceship Interface

 This sketch is written to accompany Project 2 in the
 Arduino Starter Kit

 Parts required:
 1 green LED
 2 red LEDs
 pushbutton
 10 kilohm resistor
 3 220 ohm resistors

 Created 13 September 2012
 by Scott Fitzgerald
 
 Modifed 21 April 2015
 by TC

 http://arduino.cc/starterKit

 This example code is part of the public domain
*/


static int CRUISE = 0;
static int INIT = 1;
static int ENGAGE = 2;

// initial 'jump_drive' state.
int jump_drive = CRUISE;

// the delay for blink time in ms.
static int BLINK_TIME = 2000;
// the last blink time.
unsigned long blink_clock = 0;
// standby state blink time.
boolean blink_state = true;

// the delay for init time in ms.
static int INIT_TIME = 4000;
// the last init time.
unsigned long init_clock = 0;

// the delay for blink time in ms.
static int JUMP_TIME = 8000;
// the last jump time.
unsigned long jump_clock = 0;

void setup() {

  Serial.begin(9600);
  Serial.println("Initialised stream...");
  
  // declare the LED pins as outputs
  pinMode(3, OUTPUT);
  pinMode(4, OUTPUT);
  pinMode(5, OUTPUT);

  // declare the switch pin as an input
  pinMode(2, INPUT);
}


void loop() {
  

  unsigned long current_time = millis();
  // Serial.print("Time: " + current_time);
  
  
  int switchstate = digitalRead(2);
  
  // Button Pressed
  if (switchstate == HIGH) {
    jump_drive = INIT;
    init_clock = millis();
  }

  if (jump_drive == CRUISE) {
    digitalWrite(3, HIGH);    // turn the green LED on pin 3 on
    update_blink_state();
    if (blink_state) {
        digitalWrite(4, LOW);   // turn the red LED on pin 4 off
        digitalWrite(5, LOW);   // turn the red LED on pin 5 off
    }
    else {
      digitalWrite(4, HIGH);  // turn the red LED on pin 4 on
      digitalWrite(5, HIGH);  // turn the red LED on pin 5 on
    }    
  }
  
  else if (jump_drive == INIT) {
    if (current_time - init_clock < INIT_TIME) {
      digitalWrite(3, HIGH);  // turn the green LED on pin 3 on
      digitalWrite(4, HIGH);  // turn the red LED on pin 4 on
      digitalWrite(5, LOW);   // turn the red LED on pin 5 off
    }
    else {
      jump_drive = ENGAGE;
      jump_clock = millis();
    }
  }
  
  else if (jump_drive == ENGAGE) {
    if (current_time - jump_clock < JUMP_TIME) {
      digitalWrite(3, LOW);   // turn the green LED on pin 3 off
      digitalWrite(4, LOW);   // turn the red LED on pin 4 off
      digitalWrite(5, HIGH);  // turn the red LED on pin 5 on
    }
    else {
      jump_drive = CRUISE;
      blink_state = true;
      blink_clock = millis();
    }  
  }
  else {
    jump_drive = CRUISE;
    blink_state = true;
    blink_clock = millis();
  }
  
}


void update_blink_state() {
  unsigned long current_time = millis();
  if (current_time - blink_clock > BLINK_TIME) {
    // Serial.println("Switching blink state." + current_time);
    Serial.println("Switching blink state.");
    blink_clock = current_time;
    blink_state = !blink_state;
  }
}

