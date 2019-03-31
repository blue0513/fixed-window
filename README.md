# Fixed Window

Open a file with the customizable fixed-width-window.

## What's This

This package can open a file with the customizable _fixed-window_ (fixed width window).  
Optionally it can disable to switch from other-window, make itself dedicated, disable truncate etc.

[![Image from Gyazo](https://i.gyazo.com/9e379fc260fd4655d8bc27d2654f9eed.gif)](https://gyazo.com/9e379fc260fd4655d8bc27d2654f9eed)

## Setup

`git clone` and edit your init.el as below.

```elisp
(add-to-list 'load-path "YOUR PATH")
(require 'fixed-window)
```

## Usage

### Create fixed-window with a file

`M-x fixed-window-create` then choose a file to open.

### Delete fixed-window

`M-x fixed-window-delete` then the fixed-window will be deleted.

### Jump to fixed-window

`M-x fixed-window-goto`  
This function is useful when you set `(setq fixed-window-disable-switch t)` .

## Customize

|variable|usage|default value|
|:---:|:---:|:---:|
|fixed-window-disable-switch|Disable to switch to the fixed-window by `other-window'|t|
|fixed-window-disable-truncate|Disable to truncate in the fixed-window|nil|
|fixed-window-dedicated|Make fixed-window dedicated|t|
|fixed-window-width-ratio|Width ratio of fixed-window|0.85|
