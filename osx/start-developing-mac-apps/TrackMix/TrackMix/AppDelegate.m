//
//  AppDelegate.m
//  TrackMix
//
//  Created by Temple Cloud on 31/12/2013.
//  Copyright (c) 2013 Ripoto. All rights reserved.
//

#import "AppDelegate.h"
#import "Track.h"

@implementation AppDelegate

// Implemented Public Methods

// |*| : Initialise the MVC Model state.
//
- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    Track *aTrack = [[Track alloc] init];
    [self setTrack:aTrack]; // self.track = aTrack;
    
    [self.track setVolume:5.0];
    [self updateUserInterface];
    
    NSLog(@"Initialised: applicationDidFinishLaunching");
}

// |*| : Set the new 'mute' volume incoming volume alteration events form the UI.
//
- (IBAction)mute:(id)sender {
    NSLog(@"received a mute: message");
    [self.track setVolume:0.0];
    [self updateUserInterface];
}


// |*| : Set the new volume incoming volume alteration events form the UI.
//
- (IBAction)takeFloatValueForVolume:(id)sender {
    
    [self logTakeFloatValueForVolume:sender];
    
    float newValue = [sender floatValue];
    [self.track setVolume:newValue];
    [self updateUserInterface];
}


// |*| : Set the new volume incoming volume alteration events form the UI.
//
- (void)updateUserInterface {
    
    float volume = [self.track volume];
    [self.volumeTextField setFloatValue:volume];
    [self.volumeSlider setFloatValue:volume];
}


// Private Methods

// |*| : Log incoming volume alteration events form the UI.
//
- (void)logTakeFloatValueForVolume:(id)sender {
    NSString *senderName = nil;
    
    if (sender == self.volumeTextField) {
        senderName = @"textField";
    }
    else if (sender == self.volumeSlider) {
        senderName = @"slider";
    }
    else {
        // Exceptions?
    }
    
    NSLog(@"%@ sent takeFloatValueForVolumeFrom: with value %1.2f", senderName, [sender floatValue]);
}

@end
