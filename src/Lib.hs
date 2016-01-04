{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import GHCJS.Foreign ()
import GHCJS.Types
import GHCJS.DOM

import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.Maybe (maybe)

import Reflex
import Reflex.Dom
import qualified Data.Map as Map

import Safe (readMay)

foreign import javascript unsafe "window.alert($1)" js_alert :: JSString -> IO ()

someFunc :: IO ()
someFunc = mainWidget $ el "div" $ do
    nx <- numberInput
    d <- dropdown "*" (constDyn ops) def
    ny <- numberInput
    values <- combineDyn (,) nx ny
    result <- combineDyn (\o (x, y) -> stringToOp o <$> x <*> y) (_dropdown_value d) values
    resultString <- mapDyn show result
    text " = "
    dynText resultString

ops = Map.fromList . map (join (,)) $ ["+", "-", "*", "/"]
stringToOp "+" = (+)
stringToOp "-" = (-)
stringToOp "/" = (/)
stringToOp "*" = (*)

numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
numberInput = do
    let validState = Map.singleton "style" "border-color: green"
        errorState = Map.singleton "style" "border-color: red"
    rec n <- textInput
            $ def & textInputConfig_inputType .~ "number"
                  & textInputConfig_initialValue .~ "0"
                  & textInputConfig_attributes .~ attrs
        result <- mapDyn readMay $ _textInput_value n
        attrs <- mapDyn (maybe errorState (const validState)) result
    return result
