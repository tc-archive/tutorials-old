//
//  AppDelegate.h
//  TrackMix
//
//  Created by Temple Cloud on 31/12/2013.
//  Copyright (c) 2013 Ripoto. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class Track;

@interface AppDelegate : NSObject <NSApplicationDelegate>

// MVC - Model
@property (strong) Track *track;

// MVC - View Components
@property (assign) IBOutlet NSWindow *window;
@property (weak) IBOutlet NSTextField *volumeTextField;
@property (weak) IBOutlet NSSlider *volumeSlider;

- (IBAction)mute:(id)sender;
- (IBAction)takeFloatValueForVolume:(id)sender;
- (void)updateUserInterface;


@end
