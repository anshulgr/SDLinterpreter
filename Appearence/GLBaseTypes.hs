module Appearence.GLBaseTypes
%%% This file is part of VOP, (c) 2002 Wolfgang Thaller.
%%% Please read the file LICENSE.tex.

% {code}
\chapter{Module GLBaseTypes}
This module just reexports a few basic types from HOpenGL. It is intended to
reduce the dependency on OpenGL for the parts of VOP that are not directly
concerned with rendering.
\begin{code}
module GLBaseTypes(
		Vertex3(..), Normal3(..), Vector3(..),
		GLmatrix(..), GLcolumn4(..),
		Color3(..),
		VertexMode(..),
	) where

import GL
\end{code}
