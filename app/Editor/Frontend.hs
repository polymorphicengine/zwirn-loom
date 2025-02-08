module Editor.Frontend where

{-
    Frontend.hs - defines the html dom for the editor interface
    Copyright (C) 2025, Martin Gius

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import Control.Monad (void)
import Editor.UI
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)
import System.Environment (getExecutablePath)
import System.FilePath (dropFileName)

frontend :: Window -> UI ()
frontend win = do
  void $ return win # set title "zwirn-loom"

  setCallBufferMode NoBuffering

  body <- UI.getBody win

  void $ element body #+ [container]

container :: UI Element
container = UI.div #. "container" #+ [editorContainer, messageContainer]

editorContainer :: UI Element
editorContainer = UI.div #. "editor-container" #@ "editor-container" -- #+ [editor]

editor :: UI Element
editor = UI.div #+ [UI.textarea #@ "editor0"]

messageContainer :: UI Element
messageContainer = UI.div #. "message-container" #@ "message-container"

settings :: UI Element
settings = do
  execPath <- liftIO $ dropFileName <$> getExecutablePath
  tidalKeys <- liftIO $ readFile $ execPath ++ "static/zwirnKeys.js"
  mkElement "script" # set UI.text tidalKeys

fileInput :: UI Element
fileInput =
  UI.input
    # set UI.id_ "fileInput"
    # set UI.type_ "file"
    # set style [("display", "none")]
