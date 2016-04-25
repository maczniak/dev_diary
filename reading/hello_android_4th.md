# [Hello, Android (Fourth Edition)][homepage], by Ed Burnette, The Pragmatic Programmers (2015)

[homepage]: https://pragprog.com/book/eband4/hello-android

### Preface

this book covers 4.1-5.0, 5.1, does not cover NDK<br>
[Android Device Dashboard][android_device_dashboard]

[android_deice_dashboard]: http://d.android.com/resources/dashboard/platform-versions.html

## Part I - Introducing Android

### 1. Quick Start

install JDK 7.0+ and Android Studio (from May 2013, use IntelliJ IDEA and Gradle instead of Eclipse IDE)<br>
[command-line tools][command_line_tools]<br>
use "Basic Activity" in a example application<br>
alternative emulator [Genymotion][genymotion]<br>
Android 4.2+ Settings application > About phone or tablet > tap on the build number seven times, then Developer options > Debugging > USB debugging, [Using Hardware Devices][using_hardware_devices]<br>
leave the emulator window running to save the emulator boot time<br>
Preferences > Appearance & Behavior > System Settings > Updates (automatic update)<br>
Preferences > Appearance & Behavior > System Settings > SDK Manager, install these:
* Android SDK Tools
* Android SDK Platform-tools - low-level tools like adb (Android Debug Bridge)
* Android SDK Build-tools
* Android 5.1 (API 22) or later
* Extras category
 * Android Support Repository - for gradle
 * Android Support Library - for compatibility with older Android versions
 * Google Play services
 * Google Repository - for gragle
 * Google USB Driver (Windows only)
 * Intel x86 Emulator Accelerator
if network problems occur, check Tools > Options > Force https://... sources to be fetched using http://...<br>
[IntelliJ IDEA Quick Start guide][intellij_guide]

[command_line_tools]: http://d.android.com/tools/help
[genymotion]: http://www.genymotion.com
[using_hardware_devices]: http://d.android.com/tools/device.html
[intellij_guide]: http://www.jetbrains.com/idea/documentation

### 2. Key Concepts

`add shell` opens a Linux shell<br>
[Native Development Toolkit][ndk] (NDK)<br>
ART (Android Runtime) - replace Dalvik (by Dan Bernstein) from Android 5.0 (Lollipop), ahead-of-time compiler, longer install time<br>
building blocks
* activity - user interface screen, save its own state, get application global information by extending Context class
* fragment - component of an activity, you need to use a compatibility library prior to Android 3.0 (Honeycomb)
* view - the smallest level of user interface, Java code or XML layout, have attributes
* intent - describe a specific action, replacable
* service - run in the background, many built-in services with convenient APIs
* content provider - data wrapped up in the custom API, share global data between applications
* resource - in `res` directory, `R` class with identifiers (different from standard Java resources), [Android resource compiler][aapt] (aapt)
Activity Manager manages the application stack that is navigated by using Back button.<br>
applicaion = activities + one Linux process (disposable containers for activities), application can be "alive" even if its process has been killed<br>
each activity and fragment has its own life cycle.<br>
activity life cycle (running -> paused -> stopped)
* starting
* running - OnCreate(Bundle) or OnRestart() -> onStart() about to de displayed -> (only from starting) optional override onRestoreInstanceState(Bundle) -> onResume() start interacting with the user
* paused - optional override onSaveInstanceState(Bundle) -> onPause()
* stopped - optional override onSaveInstanceState(Bundle) -> may not be run onStop()
* destroyed - may not be run onDestroy()
fragment life cycle (fragments can outlive the activities that contain them)
* starting
* created - onInflate(), onAttach(), onCreate()
* active - onCreateView(), onActivityCreated(), onViewStateRestored(), onStart()*, onResume()*
* inactive - onPause()*, onStop()*, onSaveInstanceState()*, onDestroyView()
* destroyed - onDestroy(), onDetach()
applicaion's `AndroidManifest.xml` asks for permissions. when the application is installed, Package Manager grants permissions based on certificates or user answers.<br>
permissions - INTERNET, READ_CONTACTS, WRITE_CONTACTS, RECEIVE_SMS (monitor incoming SMS messages), ACCESS_COARSE_LOCATION (use cell tower or Wi-Fi), ACCESS_FINE_LOCATION (use GPS)<br>
refer to [Android security model][android_security_model] for details

[ndk]: http://d.android.com/tools/sdk/ndk
[aapt]: http://d.android.com/tools/building
[android_security_model]: http://d.android.com/training/articles/security-tips.html

## Part II - Let's Play a Game

### 3. Opening Moves

Project, Packages, Android mode (drop-down menu next to the window name)<br>
[layouts][layout_list] - FrameLayout (show one of views), GridLayout, LinearLayout, RelativeLayout<br>
android:layout_width and android:layout_height are mandatory. possible values - "match_parent", "wrap_content", absolute width<br>
tools:context=".TicTacToeActivity" is not used, just for the visual editor<br>
refer to [View class][view_class] for the usr interfac element list<br>
AlertDialog.Builder builder; builder.setMessage(...); builder.setPositiveButton(...); AlertDialog dialog = builder.create/show();<br>
Activity.getString(...), @string/... in XML<br>
[drawable types and attribues list][drawable]<br>
press Ctrl+Space at the end of "android:theme" in AndroidManifest.xml to see the theme list<br>
dimen - px, in, mm, pt = 1/72 in, dp=dip (density-independent pixels, 1dp = 1px in 10 dots per inch), sp (scale-independent pixels, scaled by the user's font size preference)<br>
[Android design guidelines][design_guideline]<br>
stop activity - Back button, Home button, swipe after Recent Apps button
logging - Log.e/w/i/d/v(), Log.wtf() what a terrible failure, LogCat view or platform-tools/"adb logcat" command<br>
automated test - JUnit based unit test, [user interface test][ui_test], [monkey test][monkey_test]

[layout_list]: http://d.android.com/guide/topics/ui/declaring-layout.htm
[view_class]: http://d.android.com/reference/android/view/View.html
[drawable]: http://d.android.com/guide/topics/resources/drawable-resource.html
[design_guideline]: http://d.android.com/design
[ui_test]: http://d.android.com/tools/testing/testing_ui.html
[monkey_test]: http://d.android.com/tools/help/monkey.html

### 4. Defining the Game Interface

drawable-xxhdpi (extra extra high, 400 dots per inch)<br>
Intent intent = new Intent(getActivity(), GameActivity.class); intent.putExtra(GameActivity.KEY_RESTORE, true); getActivity().startActivity(intent);<br>
Alt+Enter (fix errors)<br>
AndroidManifest.xml must contain all activities.<br>
Activity methods - .getFragmentManager(), .getIntent(), .getPreferences(MODE_PRIVATE) for small data, use SQLite for large data<br>
Fragment methods - .putState(...), setRetainInstance(true) do not destroy this fragment when the parent activity is destroyed on a configuration change (such as rotating the device)

### 5. Ghost in the Machine

### 6. Adding Sounds

### 7. Adding Animation

## Part III - Thinking Outside the Box

### 8. Write Once, Test Everywhere

Version             | Code name              | API | Released  | Comments | preface comments
:------------------ | :--------------------- | :-- | :-------- | :------- | :---------------
1.0*                | BASE                   | 1   | Sep 2008  | First version |
1.1*                | BASE_1_1               | 2   | Feb 2009  | Attachments, Marquee |
1.5*                | CUPCAKE                | 3   | May 2009  | Widgets, Virtual keyboards |
1.6*                | DONUT                  | 4   | Sep 2009  | High- and low-density displays |
2.0*                | ECLAIR                 | 5   | Now 2009  | Exchange accounts |
2.0.1*              | ECLAIR_0_1             | 6   | Dec 2009  | Multi-touch |
2.1*                | ECLAIR_MR1             | 7   | Jan 2010  | Live wallpaper |
2.2*                | FROYO                  | 8   | May 2010  | SD card installs |
2.3*                | GINGERBREAD            | 9   | Dec 2010  | Native gaming | last of old generation, phone-only
2.3.3               | GINGERBREAD_MR1        | 10  | Feb 2011  | NFC |
3.0*                | HONEYCOMB              | 11  | Feb 2011  | Fragments, ActionBars, Holo theme | major departure, tablet-only, limited adoption
3.1*                | HONEYCOMB_MR1          | 12  | May 2011  | USB API, Joysticks |
3.2*                | HONEYCOMB_MR2          | 13  | June 2011 | 7" screens |
4.0*                | ICE_CREAM_SANDWICH     | 14  | Oct 2011  | Roboto, Unified phone/tablet UI | combine phone and tablet, very little additional functionality
4.0.3               | ICE_CREAM_SANDWICH_MR1 | 15  | Dec 2011  | Social stream API |
[4.1][changes_16]   | JELLY_BEAN             | 16  | Jun 2012  | Project Butter (code name), sys trace, Expandable notifications | improve usability and performance
[4.2][changes_17]   | JELLY_BEAN_MR1         | 17  | Nov 2012  | Multi-user, wireless display (Miracast standard) | performance improvement
[4.3][changes_18]   | JELLY_BEAN_MR2         | 18  | Jul 2013  | OpenGL ES 3.0, SELinux, Restricted profiles |
[4.4][changes_19]   | KITKAT                 | 19  | Oct 2013  | Chromium WebView, Immersive mode |
[4.4W*][changes_20] | KITKAT_WATCH           | 20  | Jun 2014  | Android Wear |
[5.0][changes_21]   | LOLLIPOP               | 21  | Oct 2014  | ART, Material design, Project Volta (improve battery life) |
[5.1][changes_22]   | LOLLIPOP_MR1           | 22  | Mar 2015  | Multiple SIM cards, Carrier services (provision apps through Google Play) | deprecate AndroidHttpClient and org.apache.http classes
* no longer in use

[changes_16]: http://d.android.com/sdk/api_diff/16/changes.html
[changes_17]: http://d.android.com/sdk/api_diff/17/changes.html
[changes_18]: http://d.android.com/sdk/api_diff/18/changes.html
[changes_19]: http://d.android.com/sdk/api_diff/19/changes.html
[changes_20]: http://d.android.com/sdk/api_diff/20/changes.html
[changes_21]: http://d.android.com/sdk/api_diff/21/changes.html
[changes_22]: http://d.android.com/sdk/api_diff/22/changes.html

### 9. Publishing to the Play Store

## Part IV - Beyond the Basics

### 10. Connecting to the World

### 11. Calling Web Services

### 12. Using Google Play Services

### 13. Putting SQL to Work

## Part V - Appendixes

### A1. Java vs. the Android Language and APIs

