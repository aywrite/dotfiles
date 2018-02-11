#!/bin/bash

# brew casks to install on new computer

# Install Caskroom
brew tap caskroom/cask
brew install brew-cask
brew tap caskroom/versions

# Install packages
apps=(
    1password
    iterm2
    firefox
    google-chrome
    spotify
)

brew cask install "${apps[@]}"
