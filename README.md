# ytdious
`ytdious` is an experimental YouTube "frontend" for Emacs. You can search and list YouTube videos sort them and play them either one by one or continiously. `ytdious` can be used to launch operations on videos with external tools, to learn how to do that refer to the [usage](#usage) section below.

![demonstration](pic/demonstration.gif)

This project uses the [Invidious](https://github.com/omarroth/invidious) API.

## Installation
This project is on [MELPA](https://melpa.org/): you should be able to `M-x package-install RET ytdious`. Another option is to clone this repository under your `load-path`.

### Dependencies
While `ytdious` does not depend on any Emacs package it does depend on `curl` so, if you happen not to have it, install it through your package manager (meme distros aside it is probably in your repos).

## Usage
Once everything is loaded `M-x ytdious` creates a new buffer and puts it in `ytdious-mode`. This major mode has just a few bindings (for now):

| key                 | binding                         |
|---------------------|---------------------------------|
| <key>q</key>        | `ytdious-quit`                  |
| <key>d</key>        | `ytdious-rotate-date`           |
| <key>D</key>        | `ytdious-rotate-date-backwards` |
| <key>r</key>        | `ytdious-rotate-sort`           |
| <key>R</key>        | `ytdious-rotate-sort-backwards` |
| <key>o</key>        | `ytdious-toggle-sort-direction` |
| <key>t</key>        | `ytdious-display-full-title`    |
| <key>s</key>        | `ytdious-search`                |
| <key>S</key>        | `ytdious-search-recent`         |
| <key>c</key>        | `ytdious-view-channel`          |
| <key>C</key>        | `ytdious-view-channel-at-point` |
| <key>w</key>        | `ytdious-copy-url-at-point`     |
| <key>></key>        | `ytdious-search-next-page`      |
| <key><</key>        | `ytdious-search-previous-page`  |
| <key>RET</key>      | `ytdious-play`                  |
| <key>C-return</key> | `ytdious-play-continiously`     |
| <key>C-escape</key> | `ytdious-stop-continiously`     |

Pressing `s` will prompt for some search terms or `c` for a channel name ** and populate the buffer once the results are available. One can access information about a video via the function `ytdious-get-current-video` that returns the video at point.
** channel name can't include spaces but you can use the UCID

You can create a buffer or file with content like that with a optional date limiter:
```
Linux date:week
Emacs date:today
```

mark a line and start ytdious-region-search on them, so that you don't have to remember and don't have to manually input all your searches. Also you can keep open 1 buffer per search and operate them in parralel.

Moreover, You can implement functions to:
- open a video in the browser,
- download a video,
- download just the audio of a video,

by relying on the correct external tool.

It is also possible to customize the sorting criterion of the results by setting the variable `ytdious-sort-criterion` to one of the following symbols `relevance`, `rating`, `upload_date` or `view_count`.
The default value is `relevance`.

## Potential problems and potential fixes
`ytdious` does not use the official YouTube APIs but relies on the [Invidious](https://github.com/omarroth/invidious) APIs (that in turn circumvent YouTube ones). The variable `ytdious-invidious-api-url` points to the invidious instance (by default `https://invidio.us`) to use, this server is not up anymore therefor you have to choose [another instance](https://github.com/omarroth/invidious#invidious-instances). Sometimes `ytdious` might hang; in that case `C-g` and retry.

## Contributing
Feel free to open an issue or send a pull request, help is appreciated.
To prevent redundant work I suggest to give me a heads up first, I don't always push out changes for immediately till I am happy with the quality and have it tested for a while, I could add then a TODO list or we can coordinate through bug reports.

## FAQ

#### Why?
One can easily subscribe to YouTube channels via an RSS feed and access it in Emacs via [elfeed](https://github.com/skeeto/elfeed/) but sometimes I want to search YouTube for videos of people I don't necessarily follow (e.g. for searching a tutorial, or music, or wasting some good time) and being able to do that without switching to a browser is nice.

#### What about [helm-youtube](https://github.com/maximus12793/helm-youtube) and [ivy-youtube](https://github.com/squiter/ivy-youtube)?
First of all those packages require you to get a Google API key, while `ytdious` uses the [Invidious](https://github.com/omarroth/invidious) APIs that in turn do not use the official Google APIs.

Moreover those packages are designed to select a YouTube search result and play it directly in your browser while `ytdious` is really a way to collect search results in an `elfeed`-like buffer and make them accessible to the user via functions such as `ytdious-get-current-video` so the user gets to decide what to to with them.
