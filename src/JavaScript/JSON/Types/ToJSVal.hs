{-# OPTIONS_GHC -fno-warn-orphans #-} -- The ToJSVal instance here is an orphan
module JavaScript.JSON.Types.ToJSVal where

import qualified Data.Aeson as AE
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
-- GHCJS
import qualified Data.JSString.Text as JSS
import GHCJS.Types
import GHCJS.Foreign.Internal
import GHCJS.Marshal.Internal
import qualified JavaScript.Array.Internal  as AI
import qualified JavaScript.Object.Internal as OI

toJSVal_aeson :: AE.ToJSON a => a -> IO JSVal
toJSVal_aeson x = cv (AE.toJSON x)
  where
    cv = convertValue

    convertValue :: AE.Value -> IO JSVal
    convertValue AE.Null       = return jsNull
    convertValue (AE.String t) = return (pToJSVal t)
    convertValue (AE.Array a)  = (\(AI.SomeJSArray y) -> y) <$>
                                 (AI.fromListIO =<< mapM convertValue (V.toList a))
    convertValue (AE.Number n) = toJSVal (realToFrac n :: Double)
    convertValue (AE.Bool b)   = return (toJSBool b)
    convertValue (AE.Object o) = do
      obj@(OI.Object obj') <- OI.create
      mapM_ (\(k,v) -> convertValue v >>= \v' -> OI.setProp (JSS.textToJSString k) v' obj) (H.toList o)
      return obj'

instance ToJSVal AE.Value where
    toJSVal = toJSVal_aeson
    {-# INLINE toJSVal #-}
