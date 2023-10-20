# Samořešítko pro wordle

## How to run

```sh
git clone https://github.com/Bleskocvok/wordlitko.git
cd wordlitko
```

1. Prerequisites

- `GNU Make`
- `GHC 9.0.1+`
- `Python 3.8+`
    - package `selenium`
    - package `Tkinter`
- `Firefox`
- `FirefoxDriver` (https://github.com/mozilla/geckodriver/releases)

2. Unzip the webdriver to the folder `driver/`.

3. Run using `make` to automatically launch the solver with appropriate parameters.
```sh
make run
```

### Windows

3. Try running using. No guarantees.
```sh
make.exe run
```
