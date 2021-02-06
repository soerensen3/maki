# What is Maki

Maki is a templating engine for [Free Pascal](https://www.freepascal.org) based on nodes (aka. graphs). Its main purpose is to produce text based outputs with the possibility of extending the engine to create different kinds of outputs. If you are not familiar with the concept of nodes you can have a look at [Blender](https://www.blender.org), where this feature is used extensively. In this free 3D creation suite you can alter the appearance of 3D objects by [creating materials purely by connecting nodes](https://docs.blender.org/manual/en/2.79/render/blender_render/materials/nodes/introduction.html) and without the knowledge of a programming language. For example you can take a texture node and connect it to the color input of a material node to get a textured object.

My main motivation for this project was to create a material designer similar to the one in Blender in free pascal. A material in real-time rendering is usually defined by a shader based on a source code and thus a text file. Since Maki is not limited to the use with shaders it can be used for any other purpose where you want to create text files using nodes (E.g. visual programming). It can also be used as a template engine for source code or writing or for producing mail merge (It does not yet have batch capabilites but can be extended easily) or automating tasks.

# Where can I get the source

For now it is not released to the public because I want to define an api first. There is already the compiler and a working version which is not very user friendly however. There are a lot of great open source projects whose only problem is that they lack documentation. This time I want to start with writing the documentation while I design the underlying Maki language. I publish this documentation in advance because **I want to get feedback** in early phase. 

# Contributing

If you have any suggestions or ideas you are very welcome to create a ticket in the issues tracker. 

# Documentation

* [Getting Started](doc/maki-getting-started.md).
