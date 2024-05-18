
# Haskell Chip8 Emulator

This is a Chip8 emulator written in Haskell. Chip8 is an interpreted programming language used in the late 1970s and early 1980s for simple video games. This emulator allows you to run Chip8 programs on modern systems.

![](docs/chip8.gif)

## Development Prerequisites

Before you can start developing on this emulator, you need to set up your development environment. Follow these steps:

### 1. Install Haskell Tools

To get started, you'll need GHC (Glasgow Haskell Compiler) and SDL2 for graphics. Here's how:

#### Install GHC with `ghcup`:

The recommended way to install GHC is by using `ghcup`, the Haskell compiler manager. Follow these steps:

- Install `ghcup` by following the instructions on the [official GitHub repository](https://github.com/haskell/ghcup).
- Once `ghcup` is installed, open a terminal and run:

  ```
  ghcup install ghc 9.8.2
  ```

  This will install GHC version 9.8.2 on your system.

- After installing GHC, set it as the default GHC version by running:

  ```
  ghcup set ghc 9.8.2
  ```

#### Install SDL2:

If you're using Ubuntu, you can install SDL2 by running:

```
sudo apt-get update && sudo apt-get install libsdl2-dev -y
```

For other operating systems, please refer to the corresponding instructions.

### 2. Clone the Repository

Clone this repository to your local machine using Git:

```
git clone https://github.com/yourusername/haskell-chip8-emulator.git
```

### 3. Build the Project

Navigate to the root directory of the cloned repository in your terminal and run the following command to configure and build the project using Cabal:

```
cabal build
```

This will download and install the necessary Haskell packages specified in the project's `.cabal` file and build the project.

### 4. Run the Project

After the project has been built successfully, you can run the emulator. You have two options:

- Use `cabal run`, which will open the default test ROM that just displays a logo.
- Or run a specific ROM by executing:

  ```
  cabal exec chip <path_to_rom>
  ```

  For example:

  ```
  cabal exec chip roms/test-6-keypad.ch8
  ```

## Contributing

Contributions are welcome! If you find any issues or have ideas for new features, please open an issue or create a pull request. Make sure to follow the project's coding conventions and guidelines.
