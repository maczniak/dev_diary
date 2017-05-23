# [iOS 10 App Development Essentials][homepage] by Neil Smyth, eBookFrenzy (2016)

[source code download request form][source_code_download_request_form],
 [errata][errata]<br>
iOS 10, Xcode 8, Swift 3

[source_code_download_request_form]: http://www.ebookfrenzy.com/retail/ios10/
[homepage]: http://www.ebookfrenzy.com/ebookpages/ios_10_ebook.html
[errata]: http://www.ebookfrenzy.com/errata/ios10.html

## 1. Start Here

## 2. Joining the Apple Developer Program

Fortunately this is no longer the case and all that is now required to test apps
 on physical iOS devices is an Apple ID.<br>
Of particular significance is the fact that iCloud access, Apple Pay, Game
 Center and In-App Purchasing can only be enabled and tested with Apple
 Developer Program membership. Of futher significance is the fact that Apple
 Developer Program members have access to technical support from Apple's iOS
 support engineers (though the annual fee initially covers the submission of
 only two support incident reports more can be purchased) and membership of the
 Apple Developer forums. Program membership also provides early access to the
 prelease Beta versions of both Xcode and iOS. Membership is a mandatory
 requirement in order to publish an application for sale or download in the App
 Store.

## 3. Install Xcode 8 and the iOS 10 SDK

The Xcode 8 environment requires that the version of macOS running the system be
 version 10.11.5 or later.

## 4. A Guided Tour of Xcode 8

Sticker Pack Application, iMessage Application?

## 5. An Introduction to Xcode 8 Playgrounds

The rich text (`//:` and `/*:`) uses the
 [Markdown markup language][markdown_syntax]. To display in *rendered markup*
 format, select the *Editor → Show Rendered Markup* menu option.<br>
A playground can consist of multiple pages. Another feature of playgrounds is
 the ability to bundle and access resources such as image files in a
 playground. It is also possible to test dynamic user interface behavior within
 a playground using the Xcode Enhanced Live Views feature.
 `import PlaygroundSupport`, `PlaygroundPage.current.liveView = container`

[markdown_syntax]: https://developer.apple.com/library/content/documentation/Xcode/Reference/xcode_markup_formatting_ref/

## 6. Swift Data Types, Constants and Variables

Prior to the introduction of iOS 8, the stipulated programming language for the
 development of iOS applicaions was Objective-C.<br>
Internally characters in Swift are stored in the form of *grapheme
 clusters*.<br>
A value may only be assigned to a constant once.<br>
Upcasting is performed using the *as* keyword and is also referred to *as*
 *guaranteed conversion* since the compiler can tell from the code that the cast
 will be successful. Downcasting is performed using the *as!* keyword syntax and
 is also referred to as *forced conversion*. A safer approach to downcasting is
 to perform an optional bunding using *as?*. It is also possible to *type check*
 a value using the *is* keyword.

```swift
let myTuple = (10, 432.433, "This is a String")
        // or (count: 10, length: 432.433, message: "This is a String")
let myString = myTuple.2
var (myInt, _, myString) = myTuple

if let firstPet = pet1, let secondPet = pet2, petCount > 1 {
   // ...
}
```

## 7. Swift Operators and Expressions

## 8. Swift Flow Control

The *repeat ... while* loop replaces the Swift 1.x *do .. while* loop.<br>
The guard statement is a Swift language feature introduced as part of Swift 2.
 The guard statement must include an *else* clause to be executed in the event
 that the expression evaluates to false. The code in the else clause must
 contain a statement to exit the current code flow (i.e., a *return*, *break*,
 *continue* or *throw* statement). Alternatively the else block may call any
 other function or method that does not itself return. The guard statement
 essentially provides an "early exit" strategy from the current function or loop
 in the event that a specified requirement is not met.

```swift
for constant_name in collection_or_range { // ...
for _ in 1...5 { // ...

func multiplyByTen(value: Int?) {
    guard let number = value , value < 10 else {
        print("Number is too high")
        return
    }
    // the unwrapped variable is available to the code outside of the guard statement.
    let result = number * 10
    print(result)
}
```

## 9. The Swift Switch Statement

```swift
switch (temperature)
{
      case 0...49 where temperature % 2 == 0:
        print("Cold and even")
        fallthrough

      default:
        break
}
```

## 10. An Overview of Swift 3 Functions, Methods and Closures

```swift
func displayStrings(_ strings: String...) { // variadic parameters

// All parameters accepted by a function are treated as constants by default.
// If changes to parameters need to be made within the function body, shadow
// copies of those parameters must be created.
func calculateArea(length: Float, width: Float) -> Float {
    var length = length

func doubleValue(_ value: inout Int) -> Int {
    value += value
     return(value)
}
print("doubleValue call returned \(doubleValue(&myValue))")
```

By default, function parameters are assigned the same local and external
 parameter names. The default external names assigned to parameters may be
 removed by preceding the local parameter names with an underscore (_)
 character.<br>
Closure expressions are self-contained blocks of code. To a large extent, and
 particularly as it relates to Swift, the terms *closure* and *closure
 expression* have started to be used interchangeably.

## 11. The Basic of Object Oriented Programming in Swift

## 12. An Introduction to Swift Subclassing and Extensions

To avoid potential initialization problems, the *init* method of the superclass
 must always be called *after* the initialization tasks for the subclass have
 been completed.<br>
That being said, subclasses still have some advantages over extensions. It is
 not possible, for example, to override the existing functionality of a class
 using an extension and extensions cannot contain stored properties.

## 13. Working with Array and Dictionary Collections in Swift

Currently only String, Int, Double and Bool data types are suitable for use as
 keys within a Swift dictionary.<br>
Removal of a key-value pair from a dictionary may be achieved either by
 assigning a *nil* value to the entry, or via a call to the *removeValueForKey*
 method of the dictionary instance.

## 14. Understanding Error Handling in Swift 3

Since the introduction of Swift 2 it is now much easier to both trigger and
 handle errors. Error types are created using values that conform to the
 ErrorType protocol and are most commonly implemented as enumerations.

```swift
func fileTransfer() throws -> Bool {
    guard connectionSpeed > 30 else {
        throw FileTransferError.lowBandwidth

func sendFile() -> String {
    defer {
        removeTmpFiles()
        closeConnection()
    }

    do {
        try fileTransfer()
    } catch FileTransferError.lowBandwidth {
        return("File Transfer Speed too Low")
    } catch { // do-catch statements must be exhaustive
        return("Unknown error")
    } // catch let error {

    try! fileTransfer // without a do-catch statement
```

## 15. The iOS 10-Application and Development Architecture

## 16. Creating an Interactive iOS 10 App

## 17. Understanding iOS 10 Views, Windows and the View Hierarchy

A useful technique for displaying the hierarchical ancestors of a view object is
 to perform a Ctrl-Shift-Click operation (or a mouse pad force touch on newer
 MacBook devices) over the object in Interface Builder.<br>
Control views inherit from the UIControl class (itself a subclass of UIView) and
 include items such as buttons, sliders and text fields.

## 18. An Introduction to Auto Layout in iOS 10

Unlike (pre-iOS 6) autosizing, Auto Layout allows contraints to be declared not
 just between a subview and superview, but between subviews. Constraints can
 also be configured to cross superview boundaries to allow, for example, two
 views with different superviews (though in the same screen) to be aligned. This
 is a concept referred to as *cross-view hierarchy constraints*.<br>
Constraints can also be explicit or variable (otherwise referred to in Auto
 Layout terminology as *equal* or *unequal*, one of <= and >=).<br>
Priorities are assigned on a scale of 0 to 1000 with 1000 representing a
 *required constraint* and lower numbers equating to *optional constraints*.<br>
Some views also have what is known as an *intrinsic content size*. When a view
 has an intrinsic content size, Auto Layout will automatically assign two
 constraints for each dimension for which the view has indicated an intrinsic
 content size preference (i.e. height and/or width). One constraint is intended
 to prevent the size of the view becoming larger than the size of the content
 (otherwise known as the *content hugging* constraint). The other constraint is
 intended to prevent the view from being sized smaller than the content
 (referred to as the *compress resistance* constraint). A view with a high
 compression resistance priority and a low content hugging priority will be
 allowed to grow but will resist shrinking in the corresponding dimension.

## 19. Working with iOS 10 Auto Layout Constraints in Interface Builder

There are "Use Auto Layout" and "Use Trait Variations" options in View →
 Utilties → Show File Inspector.<br>
Although Auto Layout is enabled by default, the Interface Builder tool does not
 automatically apply any default constraints as views are added to the layout.
 Views are instead positioned using absolute x and y coordinates.<br>
Interface Builder also uses a range of visual cues and decorations to indicate
 that constraints are either missing, ambiguous or in conflict. Valid and
 complete Auto Layout configurations are drawn using blue lines. When part of a
 layout is ambiguous the constraint lines are orange. Red constraint lines are
 used to indicate that constraints are in conflict.<br>
The layout canvas does not dynamically update the positions and sizes of the
 views that make up a user interface as constraints are added unless an option
 is selected from the *Update Frames* menu within the Auto Layout *Add New
 Constraints* or *Align* menus. To reset the view to the size and position
 dictated by the constraints when using Xcode 8.1 or later so that the canvas
 matches the runtime layout, simply select the view object and click on the
 *Update Frames* button located in the Interface Builder status bar. For Xcode
 8.0, use one of the *Update Frames* options contained within the *Resolve Auto
 Layout Issues* menu. If, on the other hand, the view had been positioned
 correctly, the current constraints could have been adjusted to match the actual
 position of the view using the *Update Contraint Constants* option of the
 *Resolve Auto Layout Issues* menu.

## 20. An iOS 10 Auto Layout Example

Instead of entering a point value into the spacing field, use the drop down menu
 to select *Use Standard Value*. This will set the spacing to a value conforming
 to Apple's user interface style guidelines for iOS apps. Note that this menu
 also provides the option to set the constraint to the top edge of the *View* or
 to the *Top Layout Guide*.

## 21. Implementing iOS 10 Auto Layout Constraints in Code

These approaches are not necessarily mutually exclusive. Furthermore, some types
 of constraint cannot yet be implemented in Interface Builder, constraints that
 cross view hierarchies being a prime example. Interface Builder is also of
 limited use when user interfaces are created dynamically at run time.<br>
In the case of a constraint that references a single view, the constraint must
 be added to the immediate parent of the view. When a constraint references two
 views, the constraint must be applied to the closest ancestor of the two
 views.<br>
It is also worth knowing that constraints initially created in Interface Builder
 can be connected to outlet properties, thereby allowing them to be referenced
 in code.

```swift
// view1.bottom = view2.bottom - 20
var myConstraint =
            NSLayoutConstraint(item: view1,
                attribute: NSLayoutAttribute.bottom,
                relatedBy: NSLayoutRelation.equal,
                toItem: view2,
                attribute: NSLayoutAttribute.bottom,
                multiplier: 1.0,
                constant: -20)
self.view.addConstraint(myConstraint)

myButton.translatesAutoresizingMaskIntoConstraints = false
```

## 22. Implementing Cross-Hierarchy Auto Layout Contraints in iOS 10

## 23. Understanding the iOS 10 Auto Layout Visual Format Language

```
[myButton1][myButton2]
[myButton(100)]
V:|-20-[myButton1(>=70@500)]-[myButton2(==myButton1)]-30-[myButton3]-|
```

```swift
let viewsDictionary = ["myLabel": myLabel, "myButton": myButton]
superview?.addConstraints(NSLayoutConstraint.constraints(
                withVisualFormat: "|-[myButton]-[myLabel(==myButton)]-|",
                options: NSLayoutFormatOptions.alignAllLastBaseline,
                metrics: nil,
                views: viewsDictionary))
```

## 24. Using Trait Variations to Design Adaptive iOS 10 User Interfaces

iOS 9 and Xcode 7 introduced the concepts of *trait variations* and *size
 classes*, intended specifically to allow a user interface layout for multiple
 screen sizes and orientations to be designed within a single storyboard file.
 These features have undergone further refinement in iOS 10 and Xcode 8.<br>
Arguably the most powerful trait category relates specifically to the size and
 orientation of the device screen. These trait values are referred to as *size
 classes*. Both the iPhone 7 and iPhone 7 Plus devices in portrait orientation,
 are represented by the compact width and regular height size class (wC hR).
 When iPhone 7 is rotated to landscape orientation the device is considered to
 be of compact height and compact width (wC hC). An iPhone 7 Plus in landscape
 orientation, on the other hand, is categorized as being of compact height and
 regular width (wR hC). In terms of size class categorization, the iPad family
 of devices (including the iPad Pro) is considered to be of regular height and
 regular width (wR hR) in both portrait and landscape orientation. A range of
 different size class settings are used when apps are displayed on the iPad
 using multitasking.

## 25. Using Storyboards in Xcode 8

Storyboard - scenes, segues<br>
One of the most common requirements when working with storyboards involves the
 transfer of data from one scene to another during a segue transition. This is
 achieved using the *prepare(for segue:)* method. Before a segue is performed by
 the storyboard runtime environment, a call is made to the *prepare* method of
 the current view controller.<br>
Instead of returning to the original instance of scene 1, however, this would
 create an entirely new instance of the ViewController class. If a user were to
 perform this transition repeatedly the application would continue to use more
 memory and would eventually be terminated by the operating system. The
 application should instead make use of the Storyboard *unwind* feature. This
 involves implementing a method in the view controller of the scene to which the
 user is to be returned and then connecting a segue to that method from the
 source view controller. To achieve this, locate scene 2 within the storyboard
 canvas and Ctrl-click and drag from the button view to the "exit" icon (the
 orange button with the white square and the right facing arrow pointing
 outward) in the panel located along the top edge of the scene view.<br>
In addition to wiring up controls in scenes to trigger a segue, it is also
 possible to initiate a preconfigured segue from within the application code.
 This can be achieved by assigning an identifier to the segue and then making a
 call to the *performSegue(withIdentifier:)* method of the view controller from
 which the segue is to be triggered.

## 26. Organizing Scenes over Multiple Storyboard Files

Storyboard Reference object in the Object Library

## 27. Using Xcode 8 Storyboards to Create an iOS 10 Tab Bar Application

## 28. An Overview of iOS 10 Table Views and Xcode 8 Storyboards

Historically, table views have been one of the more complex areas of iOS user
 interface implementation. In recognition of this fact, Apple introduced ways to
 implement table views through the use of the Xcode Storyboard feature.<br>
Dynamic tables (also known as *prototype-based* tables), on the other hand, are
 intended for use when a variable number of rows need to be displayed from a
 data source.<br>
Table views may be configured to use either *plain* or *grouped* style. Table
 Views using plain style can also be *indexed*, whereby rows are organized into
 groups according to specified criteria, such as alphabetical or numerical
 sorting.<br>
Since iOS 8, support for dynamic type has been extended to table views.<br>
If the cell is created using prototypes within a storyboard it is not necessary
 to register the class and, in fact, doing so will prevent the cell or view from
 appearing when the application runs. As the table view initializes, it calls
 the *tableView(_:cellForRowAt:)* method of the datasource class passing through
 the index path for which a cell object is required. This method will then call
 the *dequeueReusableCell* method of the table view object, passing through both
 the index path and the reuse ID assigned to the cell class when it was
 registered.

## 29. Using Xcode 8 Storyboards to Build Dynamic TableViews

## 30. Implementing iOS 10 TableView Navigation using Storyboards in Xcode 8

## 31. Working with the iOS 10 Stack View Class

## 32. An iOS 10 Stack View Tutorial

## 33. An iOS 10 Split View Master-Detail Example

## 34. A Guide to Multitasking in iOS 10

## 35. An iOS 10 Multitaskig Example

## 36. Working with Directories in Swift on iOS 10

## 37. Working with Files in Swift on iOS 10

## 38. iOS 10 Directory Handling and File I/O in Swift - A Worked Example

## 39. Preparing an iOS 10 App to use iCloud Storage

## 40. Managing Files using the iOS 10 UIDocument Class

## 41. Using iCloud Storage in an iOS 10 Application

## 42. Synchronizing iOS 10 Key-Value Data using iCloud

## 43. iOS 10 Database Implementation using SQLite

## 44. An Example SQLite based iOS 10 Application using Swift and FMDB

## 45. Working with iOS 10 Databases using Core Data

## 46. An iOS 10 Core Data Tutorial

## 47. An Introduction to CloudKit Data Storage on iOS 10

## 48. An Introduction to CloudKit Sharing

## 49. An iOS 10 CloudKit Example

## 50. An iOS 10 CloudKit Subscription Example

## 51. An iOS 10 CloudKit Sharing Example

## 52. An Overview of iOS 10 Multitouch, Taps and Gestures

## 53. An Example iOS 10 Touch, Multitouch and Tap Application

## 54. Detecting iOS 10 Touch Screen Gesture Motions

## 55. Identifying Gestures using iOS 10 Gesture Recognizers

## 56. An iOS 10 Gesture Recognition Tutorial

## 57. A 3D Touch Force Handling Tutorial

## 58. An iOS 10 3D Touch Quick Actions Tutorial

## 59. An iOS 10 3D Touch Peek and Pop Tutorial

## 60. Implementing TouchID Authentication in iOS 10 Apps

## 61. Drawing iOS 10 2D Graphics with Core Graphics

## 62. Interface Builder Live Views and iOS 10 Embedded Frameworks

## 63. An iOS 10 Graphics Tutorial using Core Graphics and Core Image

## 64. iOS 10 Animation using UIViewPropertyAnimator

## 65. iOS 10 UIKit Dynamics - An Overview

## 66. An iOS 10 UIKit Dynamics Tutorial

## 67. An Introduction to iOS 10 Sprite Kit Programming

## 68. An iOS 10 Sprite Kit Level Editor Game Tutorial

## 69. An iOS 10 Sprite Kit Collision Handling Tutorial

## 70. An iOS 10 Sprite Kit Particle Emitter Tutorial

## 71. iOS 10 Multitasking, Background Transfer Service and Fetching

## 72. An iOS 10 Local Notification Tutorial

## 73. An Overview of iOS 10 Application State Preservation and Restoration

## 74. An iOS 10 State Preservation and Restoration Tutorial

## 75. Integrating Maps into iOS 10 Applications using MKMapItem

## 76. An Example iOS 10 MKMapItem Application

## 77. Getting Location Information using the iOS 10 Core Location Framework

## 78. An Example iOS 10 Location Application

## 79. Working with Maps on iOS 10 with MapKit and the MKMapView Class

## 80. Working with MapKit Local Search in iOS 10

## 81. Using MKDirections to get iOS 10 Map Directions and Routes

## 82. An iOS 10 MapKit Flyover Tutorial

## 83. An Introduction to Extensions in iOS 10

## 84. An iOS 10 Today Extension Widget Tutorial

## 85. Creating an iOS 10 Photo Editing Extension

## 86. Creating an iOS 10 Action Extension

## 87. Receiving Data from an iOS 10 Action Extension

## 88. An Introduction to Building iOS 10 Message Apps

## 89. An iOS 10 Interactive Message App Tutorial

## 90. Using iOS 10 Event Kit to Create Date and Location Based Reminders

## 91. Accessing the iOS 10 Camera and Photo Library

## 92. An Example iOS 10 Camera Application

## 93. iOS 10 Video Playback using AVPlayer and AVPlayerViewController

## 94. An iOS 10 Multitasking Picture in Picture Tutorial

## 95. Playing Audio on iOS 10 using AVAudioPlayer

## 96. Recording Audio on iOS 10 with AVAudioRecorder

## 97. An iOS 10 Speech Recognition Tutorial

## 98. An iOS 10 Real-Time Speech Recognition Tutorial

## 99. An Introduction to SiriKit

## 100. An iOS 10 Example SiriKit Messaging Extension

## 101. An iOS 10 SiriKit Photo Search Tutorial

## 102. Integrating Twitter and Facebook into iOS 10 Applications

## 103. An iOS 10 Social Media Integration Tutorial using UIActivityViewController

## 104. iOS 10 Facebook and Twitter Integration using SLRequest

## 105. An iOS 10 Twitter Integration Tutorial using SLRequest

## 106. Making Store Purchases with the SKStoreProductViewController Class

## 107. Building In-App Purchasing into iOS 10 Applications

## 108. Preparing an iOS 10 Application for In-App Purchases

## 109. An iOS 10 In-App Purchase Tutorial

## 110. Configuring and Creating App Store Hosted Content for iOS 10 In-App Purchases

## 111. Preparing and Submitting an iOS 10 Application to the App Store

