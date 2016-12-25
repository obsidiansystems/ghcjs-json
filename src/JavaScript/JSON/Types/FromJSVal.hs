{-# OPTIONS_GHC -fno-warn-orphans #-} -- The FromJSVal instance here is an orphan
module JavaScript.JSON.Types.FromJSVal where

import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Aeson as AE
import qualified Data.HashMap.Strict as H
import Data.Scientific (Scientific, scientific, fromFloatDigits)
import qualified Data.Text.Internal as T
import qualified Data.Vector as V
-- GHCJS
import GHCJS.Foreign.Internal
import GHCJS.Marshal.Internal
import qualified JavaScript.Object.Internal as OI

instance FromJSVal AE.Value where
  fromJSVal r = case jsonTypeOf r of
    JSONNull    -> return (Just AE.Null)
    JSONInteger -> liftM (AE.Number . flip scientific 0 . (toInteger :: Int -> Integer))
         <$> fromJSVal r
    JSONFloat   -> liftM (AE.Number . (fromFloatDigits :: Double -> Scientific))
         <$> fromJSVal r
    JSONBool    -> liftM AE.Bool  <$> fromJSVal r
    JSONString  -> liftM AE.String <$> fromJSVal r
    JSONArray   -> liftM (AE.Array . V.fromList) <$> fromJSVal r
    JSONObject  -> do
        props <- OI.listProps (OI.Object r)
        runMaybeT $ do
            propVals <- forM props $ \p -> do
                v <- MaybeT (fromJSVal =<< OI.getProp p (OI.Object r))
                -- return (JSS.textFromJSString p, v)
                return (T.Text p, v)
            return (AE.Object (H.fromList propVals))
  {-# INLINE fromJSVal #-}

-- instance PFromJSVal AE.Value where
--     pFromJSVal r = case jsonTypeOf r of
--             JSONNull    -> (Just AE.Null)
--             JSONInteger -> (AE.Number . flip scientific 0 . (toInteger :: Int -> Integer)) $ pFromJSVal r
--             JSONFloat   -> (AE.Number . (fromFloatDigits :: Double -> Scientific)) $ pFromJSVal r
--             JSONBool    -> AE.Bool $ pFromJSVal r
--             JSONString  -> AE.String $ pFromJSVal r
--             JSONArray   -> AE.Array . V.fromList $ pFromJSVal r
--             JSONObject  -> fromJust $ unsafePerformIO $ do
--                 props <- OI.listProps (OI.Object r)
--                 runMaybeT $ do
--                     propVals <- forM props $ \p -> do
--                         v <- MaybeT (return . pFromJSVal =<< OI.getProp p (OI.Object r))
--                         return (JSS.textFromJSString p, v)
--                     return (AE.Object (H.fromList propVals))
--     {-# INLINE pFromJSVal #-}
