# weechat

I played around with [irssi](https://irssi.org), but ultimately settled on [weechat](https://weechat.org) as a native chat client.

`sec.conf` isn't managed in the dotfiles repo. This config doesn't run without:

```bash
/secure set bouncer <password>
/secure set clack <comma,separated,api,keys>
```

---

## Colors

* 0,  1: background (black, gray)
* 1,  9: red
* 2, 10: green
* 3, 11: yellow
* 4, 12: blue
* 5, 13: magenta (gruvbox: purple)
* 6, 14: cyan (gruvbos: aqua)
* 7, 15: light gray

*Thanks to pu on [#weechat](irc://chat.freenode.net/weechat):*

![weechat-colors](./colors.png)
