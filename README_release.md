# What is Maki

Maki is a templating engine for [Free Pascal](https://www.freepascal.org) based on nodes (aka. graphs). It can be used mainly to produce text based outputs with the possibility of extending the engine to create different kind of outputs. If you are not familiar with the concept of nodes you may have a look at [Blender](https://www.blender.org), where this feature is used extensively. In this free 3D creation suite you can alter the appearance of 3D objects by [creating materials purely by connecting nodes](https://docs.blender.org/manual/en/2.79/render/blender_render/materials/nodes/introduction.html) and without the knowledge of a programming language. For example you can take a texture node and connect it to the color input of a material node to get a textured object.

My main motivation for this project is to create a material designer similar to the one in Blender in free pascal. A material in real-time rendering is usually defined by a shader based on a source code and thus a text file. Since Maki is not limited to the use with shaders it can be used for any other purpose where you want to create text files using nodes (E.g. visual programming). It can also be used as a template engine for source code or writing or for producing mail merge (It does not yet have batch capabilites but can be extended easily).

# DISCLAIMER

This library is still in a very early phase and not to be used in production. Expect breaking changes, bugs and even crashes! It is a rewrite of my old shader compiler closely integrated with my 3D-Engine to be used for other purposes as well. Also the code is not very clean yet. Use at your own risk! Ok enough warnings!

# How to install

The recommended way is to use the [Lazarus IDE](https://www.lazarus-ide.org) in combination with Maki. Though you don't need it to use Maki it is the most common way and I will therefore only give instructions for the use with Lazarus.

## Requirements

* [storage-trees](https://github.com/soerensen3/storage-trees)

The nodes and sockets are based on the storage tree package.

## Loading the packages

To install this you can simply clone this repo and the storage-trees repo. Then load the lpk files in Lazarus and include the dependencies in your project.

# Getting started

See [Getting Started](/doc/Getting Started.md).
