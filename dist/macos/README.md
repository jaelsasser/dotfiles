# macOS Customization

This isn't scripted well, as I don't set up new MacBook's very often.


```shellsession
$ defaults write com.apple.mail NSUserKeyEquivalents -dict-add Archive '@\U007F'
$ defaults write com.apple.mail NSUserKeyEquivalents -dict-add Delete ''
$ defaults read com.apple.mail NSUserKeyEquivalents
{ 
    Archive = "@\177";
    Delete = "";
}
$ defaults read com.apple.universalaccess com.apple.custommenu.apps
(
    "com.apple.mail"
)
$ cp DefaultKeyBindings.dict ~/Library/KeyBindings/DefaultKeyBindings.dict
```

For reference:

* [Cocoa Text System](http://www.hcs.harvard.edu/~jrus/Site/Cocoa%20Text%20System.html)
* [How to Script macOS Keyboard Shortcuts](https://www.raizlabs.com/dev/2015/02/how-to-script-os-x-keyboard-shortcuts/)
