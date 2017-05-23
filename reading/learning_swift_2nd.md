# [Learning Swift: Building Apps for macOS, iOS, and Beyond][homepage] by [Jonathon Manning][jon_manning], [Paris Buttfield-Addison][paris_buttfield_addison] & [Tim Nugent][tim_nugent], O'Reilly (2017)

[source code][source_code], [Secret Lab][secret_lab]

[homepage]: http://shop.oreilly.com/product/0636920053989.do
[jon_manning]: https://twitter.com/desplesda
[paris_buttfield_addison]: https://blog.paris.id.au/
[tim_nugent]: https://twitter.com/The_McJones
[source_code]: https://github.com/thesecretlab/LearningSwift2Ed
[secret_lab]: https://twitter.com/thesecretlab

## Part I. Swift Basics

### 1. Getting Started

[Apple Developer Forums][apple_developer_forums],
 [App Distribution Guide][app_distribution_guide]<br>
The simulator is a lot faster than a real device and has a lot more memory.

[apple_developer_forums]: https://forums.developer.apple.com/
[app_distribution_guide]: https://developer.apple.com/library/content/documentation/IDEs/Conceptual/AppDistributionGuide/

### 2. The Basics of Swift

Open the Edit menu and choose Convert→To Latest Swift Syntax to get started.<br>
Importantly, constants in Swift don't have to be known at compile time. Instead,
 you can think of them as variables that are set only once.<br>
Swift 2 Versus Swift 3:
* Almost every function and property was renamed in some way. All function
  parameters now have labels unless you say otherwise; redundant words have been
  omitted; and the `NS` prefix was mostly dropped, so farewell `NSURL` and
  `UIColor.redColor()`, hello `URL` and `UIColor.red`. Additionally, Swift now
  has rules around how you should be naming your properties and functions.
* C-style `for` loop and the `++` and `--` operators are now gone.
* Declaring parts of your code as private means they can only be used when in
  the same scope as they were declared. A new access modifier called
  `fileprivate` was introduced to perform the same functionality private had in
  Swift 2.
* Swift Package Manager was released. If in the past you were using Carthage or
  Cocoapods, the Swift Package Manager works in a similar fashion.

We recommend dragging a playground file to your macOS Dock. That way, you can
 use it to test Swift code quickly and easily.<br>
Multiline comments can be nested.<br>
The `stride` function allows you to precisely control how you iterate over a
 sequence. This is the `stride(from: to: by:)` form, which is exclusive of the
 final number; there is also an inclusive form `stride(from: through: by:)`.

```swift
switch tupleSwitch {
case ("Yes", 123):
    print("Tuple contains 'Yes' and '123'")
case ("Yes", _):
    print("Tuple contains 'Yes' and something else")
case (let string, _):
    print("Tuple contains the string '\(string)' and something else")
default: // switch statements are required to be exhaustive.
    break
}

for character in "Hello".characters { // ...
```

If you really do want to see if two string variables refer to the same object,
 you use the `===` operator.<br>
Implicitly unwrapped optionals (`var foo : Int!`) are regular optionals: they
 can either contain `nil`, or not. However, whenever you access their value, the
 compiler unwraps it.<br>
You can't insert items into an array beyond its bounds. For example, if you
 tried to insert an item at element 99, it wouldn't work and would throw a
 runtime error (i.e., your program would crash).<br>
The `reverse` function doesn't reverse `myArray` but instead returns a new array
 that is the reverse of `myArray`.<br>
By default, all parameters after the first one must have a *label* associated
 with them, and the label is necessary in calling the function. However,
 sometimes you don't need a label before parameter names, especially when it's
 very obvious what the parameters are for. In these cases, you can tell Swift to
 not require a label before the parameters by placing an underscore before the
 name.<br>
You can only have a single variadic parameter, and any parameter listed after a
 variadic parameter must have an external parameter name.<br>
If you come from Objective-C land, the `in` keyword works similar to the `^`
 syntax in blocks.<br>
[API Design Guidelines][api_design_guidelines]:
* Using it like `anArray.remove(at: 2)` is clear and unambiguous, whereas if it
  were just `anArray.remove(2)` we wouldn't know if it were removing the element
  at index 2 or removing the object 2 from the array.
* Where possile, make your functions read like an English sentence;
  `anArray.insert(x at: y)` reads better than `anArray.insert(x index: y)`.
  Additionally, when making mutating and nonmutating functions, make the
  mutating functions sound like verbs and name the nonmutating form with the
  "-ed" or "-ing" suffix, so `anArray.sort()` would modify the `anArray`
  variable whereas `anArray.sorted()` would return a sorted copy.
* Finally, avoid abbreviations, acronyms, and obscure terms.

[api_design_guidelines]: https://swift.org/documentation/api-design-guidelines/

### 3. Swift for Object-Oriented App Development

To create an initializer that can return `nil`--also known as a *failable
 initializer*--put a question mark after the `init` keyword, and `return nil` if
 the initializer decides that it can't successfully construct the object.<br>
*Internal* entities (data and methods) are only accessible to the *module* in
 which they're defined. A module is an application, library, or framework. This
 is why you can't access the inner workings of UIKit--is's defined as internal
 to the UIKit framework. Internal is the default level of access control. You
 can explicitly define a member as `internal` if you want, but it isn't
 necessary.<br>
By marking something *private*, you create functionality you never want others
 to touch, even inside extensions.<br>
You can't make a method more accessible than the class in which it's
 contained.<br>
`fileprivate(set) var privateSetterProperty = 123`, `private (set)`<br>
In Swift, structures are *value types*, which are always copied when passed
 around. Some value types in Swift include `Int`, `String`, `Array`, and
 `Dictionary`, all of which are implemented as structures.<br>
(Foundation, the base layer of classes that were created to support Objective-C)
 A slightly higher-level library (than then Swift Standard Library) that
 provides more tools and types, such as `NSNotificationCenter`, which is used to
 broadcast application-wide notifications, and `JSONSerialization`, which allows
 you to read and write JSON data.<br>
You import Cocoa with the `import Cocoa` statement. You import Cocoa Touch with
 the `import UIKit` statement. If you import Cocoa or Cocoa Touch, you'll also
 import Foundation. You don't need to import both.

```swift
// The example actually calls itself in an infinitely recursive way, which hangs
//  your app.
func + (left: Int, right: Int) -> Int {
    return left + right
}

// Package.swift from https://github.com/apple/example-package-dealer
import PackageDescription

let packet = Package(
    name: "Dealer",
    dependencies: [
    .Package(
        url:"https://github.com/apple/example-package-deckofplayingcards.git",
        majorVersion: 3) // semver
    ]
)
```

[Swift Package Manager][swift_package_manager] - `swift build`,
 `swift package generate-xcodeproj`<br>
`let data = stringToConvert.data(using: String.Encoding.utf8)`<br>
You can use `Bundle` to load and unload code, images, audio, or almost anything
 imaginable without having to deal directly with the filesystem.<br>
To do this, you first make an object conform to the `NSObject` and `NSCoding`
 protocols, and then add two methods--`encodeWithCoder` and an initializer that
 takes an `NSCoder`. An object that conforms to `NSCoding` can be converted to
 an `NSData` object, and also be loaded from one, via the `NSKeyedArchiver` and
 `NSKeyedUnarchiver` classes.<br>
The `Error protocol doesn't have any required functions or properties, which
 means that any class, enum, or structure can be an error. If you preface a call
 to something that can throw an error with `try?`, and it *does* throw an error,
 the result will be `nil`.

```swift
if let URL = URL(string: "https://oreilly.com") {
    let loadedDataFromURL = try ? Data(contentsOf: URL)
    // see also AlamoFire, https://github.com/Alamofire/Alamofire
}
if let filePath = Bundle.main.path(forResource: "SomeFile", ofType: "txt") {
    let loadedDataFrom PAth = NSData(contentsOfFile:filePath)
}

class SerializableObject : NSObject, NSCoding {
    var name : String?

    func encode(with aCoder: NSCoder) {
        aCoder.encode(name!, forKey:"name")
    }

    override init() {
        self.name = "My Object"
    }

    required init(coder aDecoder: NSCoder) {
        self.name = aDecoder.decodeObject(forKey: "name") as? String
    }
}

let objectConvertedToData =
NSKeyedArchiver.archivedData(withRootObject: anObject)
let loadedObject =
NSKeyedUnarchiver.unarchiveObject(with: objectConvetedToData)
    as? SerializableObject

} catch let error as BankError {
```

The application deligate object--usually shortened to *app delegate*--is just an
 instance of a class that conforms to the `NSApplicationDelegate` (on macOS) or
 `UIApplicationDelegate` (on iOS) protocol. *Window controllers* are objects
 that manage a window's contents on macOS, and *view controllers* manage a
 view's contents on both iOS and macOS.

[swift_package_manager]: https://github.com/apple/swift-package-manager

## Part II. A macOS App

### 4. Setting Up the macOS Notes App

It uses the macOS document model, which means that it gets a number of useful
 behaviors for free, including versioning and autosave, plus the ability to
 associate its document types with the app.<br>
[Document-Based App Programming Guide for Mac][cocoa_document_architecture],
 [Core Data Programming Guide][core_data_programming_guide]<br>
The limits of Core Data are quite easy to hit, and it's often more useful, as
 well as more of a learning experience, to build storage infrastructure for your
 app from scratch.<br>
UI tests will run only on macOS 10.11 El Capitan or later.<br>
UTIs (uniform type identifiers) are a *hierarchy* of types: when a UTI is
 declared, it also declares all of the other types that is *conforms* to. For
 example, the UTI for JSON is `public.json`; this UTI also conforms to
 `public.text`, which represents *plain text* and itself conforms to both
 `public.data` and `public.content`.
 [System-Declared UTIs][system_declared_utis]<br>
The document type in this app will be one that conforms to the
 `com.apple.package` type, which means that it's a folder that contains other
 files, but should be presented to the user as a single file. macOS and iOS make
 extensive use of packages, since they're a very convenient way to present a
 file that contains other information.<br>
The 1x icon is the version of the icon for non-Retina displays, and the 2x icon
 is the version of the icon for Retina displays.
 [High Resolution Guidelines for OS X][high_resolution_guidelines_for_os_x]

[document_based_app_programming_guide_for_mac]: https://developer.apple.com/library/content/documentation/DataManagement/Conceptual/DocBasedAppProgrammingGuideForOSX/Introduction/Introduction.html
[core_data_programming_guide]: https://developer.apple.com/library/content/documentation/Cocoa/Conceptual/CoreData/
[system_declared_utis]: https://developer.apple.com/library/content/documentation/Miscellaneous/Reference/UTIRef/Articles/System-DeclaredUniformTypeIdentifiers.html
[high_resolution_guidelines_for_os_x]: https://developer.apple.com/library/content/documentation/GraphicsAnimation/Conceptual/HighResolutionOSX/Introduction/Introduction.html

### 5. Working with Documents on macOS

`NSDocument` is not part of the Swift programming language, but is instead part
 of the [AppKit framework][appkit_framework]. AppKit is the framework Apple
 provides to build graphical applications for macOS, and it contains windows,
 buttons, menus, text fields, and so on.<br>
Users of other operating systems won't see a package as a single document, but
 rather as a folder. To work with package file formats, you use the
 `FileWrapper` class.<br>
[Apple's list of possible `NSError` types][nserror_list]<br>
The triple-slash (`///`) tells Xcode to treat that comment as documentation. Put
 triple-slash comments above method names and entries in `enum`s to define what
 they mean, and Option-click those names to see this documentation.<br>
empty dictionary literal `[:]`<br>
The saving method, `fileWrapper ofType`, an `NSDocument` method we are going to
 override, is required to return an `FileWrapper` that represents a file or
 directory to be saved to disk. It's important to note that you don't actualy
 write a file yourself. / A `FileWarpper` can have multiple file wrappers inside
 it *with the same name*. We can't simply say "add a new file wrapper called
 *Text.rtf*," because if one already existed, it would be added as
 "*Text 2.rtf*," or something similar.<br>
If you're making a document-based application that stores its data in flatfiles,
 you implement the `read from data:` and `data ofType:` methods instead of the
 `read from fileWrapper:` and `fileWrapper ofType:` methods. Don't implement
 both the `FileWrapper` methods and the `Data` methods in the same class. If you
 do, you're likely to confuse the system in regards to how your documents are
 stored.<br>
The interface you build is actually the real, bona fide interface that your app
 uses, not a visual representation of it.<br>
Nib used to stand for "NeXT Interface Builder". The file format was later
 changed from a custom binary format to XML, which is why the files have the
 filename extension *.xib*. It's still referred to as "nib."<br>
[`NSTextView`][nstextview]<br>
We'll use *bindings*, which link the value of a user interface element, such as
 a text field or a label, to a property in another object. When you make changes
 to the UI element, the property is updated; when the property is updated, the
 UI element is updated as well. [Cocoa Bindings][cocoa_bindings] are available
 only on macOS. On iOS, we'd need to manually register to be notified of changes
 that the user makes.

[appkit_framework]: https://developer.apple.com/reference/appkit#//apple_ref/doc/uid/20001093
[nserror_list]: https://developer.apple.com/reference/foundation/foundation_constants#//apple_ref/doc/constant_group/NSError_Codes
[nstextview]: https://developer.apple.com/reference/appkit/nstextview
[cocoa_bindings]: https://developer.apple.com/library/content/documentation/Cocoa/Conceptual/CocoaBindings/CocoaBindings.html

### 6. User Interfaces and iCloud

`NSCollectionView` is provided by AppKit to display a grid of other views. Each
 view it displays is managed by an `NSCollectionViewItem`.

## Part III. An iOS App

### 7. Setting Up the iOS Notes App

### 8. Working with Files in iCloud

### 9. Working with Documents on iOS

### 10. Working with Files and File Types

### 11. Images and Deletion

### 12. Supporting the iOS Ecosystem

### 13. Multimedia and Location Attachments

### 14. Polishing the iOS App

## Part IV. Extending Your Apps

### 15. Building a watchOS App

### 16. Code Quality and Distribution

