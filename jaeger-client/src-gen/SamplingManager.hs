{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-----------------------------------------------------------------
-- Autogenerated by Thrift Compiler (0.11.0)                      --
--                                                             --
-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --
-----------------------------------------------------------------

module SamplingManager where
import Prelude (($), (.), (>>=), (==), (++))
import qualified Prelude as P
import qualified Control.Exception as X
import qualified Control.Monad as M ( liftM, ap, when )
import Data.Functor ( (<$>) )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import qualified Data.Int as I
import qualified Data.Maybe as M (catMaybes)
import qualified Data.Text.Lazy.Encoding as E ( decodeUtf8, encodeUtf8 )
import qualified Data.Text.Lazy as LT
import qualified GHC.Generics as G (Generic)
import qualified Data.Typeable as TY ( Typeable )
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector
import qualified Test.QuickCheck.Arbitrary as QC ( Arbitrary(..) )
import qualified Test.QuickCheck as QC ( elements )

import qualified Thrift as T
import qualified Thrift.Types as T
import qualified Thrift.Arbitraries as T


import Sampling_Types
import qualified SamplingManager_Iface as Iface
-- HELPER FUNCTIONS AND STRUCTURES --

data GetSamplingStrategy_args = GetSamplingStrategy_args  { getSamplingStrategy_args_serviceName :: LT.Text
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable GetSamplingStrategy_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` getSamplingStrategy_args_serviceName record  
instance QC.Arbitrary GetSamplingStrategy_args where 
  arbitrary = M.liftM GetSamplingStrategy_args (QC.arbitrary)
  shrink obj | obj == default_GetSamplingStrategy_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_GetSamplingStrategy_args{getSamplingStrategy_args_serviceName = getSamplingStrategy_args_serviceName obj} then P.Nothing else P.Just $ default_GetSamplingStrategy_args{getSamplingStrategy_args_serviceName = getSamplingStrategy_args_serviceName obj}
    ]
from_GetSamplingStrategy_args :: GetSamplingStrategy_args -> T.ThriftVal
from_GetSamplingStrategy_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v43 -> P.Just (1, ("serviceName",T.TString $ E.encodeUtf8 _v43))) $ getSamplingStrategy_args_serviceName record
  ]
write_GetSamplingStrategy_args :: T.Protocol p => p -> GetSamplingStrategy_args -> P.IO ()
write_GetSamplingStrategy_args oprot record = T.writeVal oprot $ from_GetSamplingStrategy_args record
encode_GetSamplingStrategy_args :: T.StatelessProtocol p => p -> GetSamplingStrategy_args -> LBS.ByteString
encode_GetSamplingStrategy_args oprot record = T.serializeVal oprot $ from_GetSamplingStrategy_args record
to_GetSamplingStrategy_args :: T.ThriftVal -> GetSamplingStrategy_args
to_GetSamplingStrategy_args (T.TStruct fields) = GetSamplingStrategy_args{
  getSamplingStrategy_args_serviceName = P.maybe (getSamplingStrategy_args_serviceName default_GetSamplingStrategy_args) (\(_,_val45) -> (case _val45 of {T.TString _val46 -> E.decodeUtf8 _val46; _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_GetSamplingStrategy_args _ = P.error "not a struct"
read_GetSamplingStrategy_args :: T.Protocol p => p -> P.IO GetSamplingStrategy_args
read_GetSamplingStrategy_args iprot = to_GetSamplingStrategy_args <$> T.readVal iprot (T.T_STRUCT typemap_GetSamplingStrategy_args)
decode_GetSamplingStrategy_args :: T.StatelessProtocol p => p -> LBS.ByteString -> GetSamplingStrategy_args
decode_GetSamplingStrategy_args iprot bs = to_GetSamplingStrategy_args $ T.deserializeVal iprot (T.T_STRUCT typemap_GetSamplingStrategy_args) bs
typemap_GetSamplingStrategy_args :: T.TypeMap
typemap_GetSamplingStrategy_args = Map.fromList [(1,("serviceName",T.T_STRING))]
default_GetSamplingStrategy_args :: GetSamplingStrategy_args
default_GetSamplingStrategy_args = GetSamplingStrategy_args{
  getSamplingStrategy_args_serviceName = ""}
data GetSamplingStrategy_result = GetSamplingStrategy_result  { getSamplingStrategy_result_success :: SamplingStrategyResponse
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable GetSamplingStrategy_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` getSamplingStrategy_result_success record  
instance QC.Arbitrary GetSamplingStrategy_result where 
  arbitrary = M.liftM GetSamplingStrategy_result (QC.arbitrary)
  shrink obj | obj == default_GetSamplingStrategy_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_GetSamplingStrategy_result{getSamplingStrategy_result_success = getSamplingStrategy_result_success obj} then P.Nothing else P.Just $ default_GetSamplingStrategy_result{getSamplingStrategy_result_success = getSamplingStrategy_result_success obj}
    ]
from_GetSamplingStrategy_result :: GetSamplingStrategy_result -> T.ThriftVal
from_GetSamplingStrategy_result record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v49 -> P.Just (0, ("success",from_SamplingStrategyResponse _v49))) $ getSamplingStrategy_result_success record
  ]
write_GetSamplingStrategy_result :: T.Protocol p => p -> GetSamplingStrategy_result -> P.IO ()
write_GetSamplingStrategy_result oprot record = T.writeVal oprot $ from_GetSamplingStrategy_result record
encode_GetSamplingStrategy_result :: T.StatelessProtocol p => p -> GetSamplingStrategy_result -> LBS.ByteString
encode_GetSamplingStrategy_result oprot record = T.serializeVal oprot $ from_GetSamplingStrategy_result record
to_GetSamplingStrategy_result :: T.ThriftVal -> GetSamplingStrategy_result
to_GetSamplingStrategy_result (T.TStruct fields) = GetSamplingStrategy_result{
  getSamplingStrategy_result_success = P.maybe (getSamplingStrategy_result_success default_GetSamplingStrategy_result) (\(_,_val51) -> (case _val51 of {T.TStruct _val52 -> (to_SamplingStrategyResponse (T.TStruct _val52)); _ -> P.error "wrong type"})) (Map.lookup (0) fields)
  }
to_GetSamplingStrategy_result _ = P.error "not a struct"
read_GetSamplingStrategy_result :: T.Protocol p => p -> P.IO GetSamplingStrategy_result
read_GetSamplingStrategy_result iprot = to_GetSamplingStrategy_result <$> T.readVal iprot (T.T_STRUCT typemap_GetSamplingStrategy_result)
decode_GetSamplingStrategy_result :: T.StatelessProtocol p => p -> LBS.ByteString -> GetSamplingStrategy_result
decode_GetSamplingStrategy_result iprot bs = to_GetSamplingStrategy_result $ T.deserializeVal iprot (T.T_STRUCT typemap_GetSamplingStrategy_result) bs
typemap_GetSamplingStrategy_result :: T.TypeMap
typemap_GetSamplingStrategy_result = Map.fromList [(0,("success",(T.T_STRUCT typemap_SamplingStrategyResponse)))]
default_GetSamplingStrategy_result :: GetSamplingStrategy_result
default_GetSamplingStrategy_result = GetSamplingStrategy_result{
  getSamplingStrategy_result_success = default_SamplingStrategyResponse}
process_getSamplingStrategy (seqid, iprot, oprot, handler) = do
  args <- read_GetSamplingStrategy_args iprot
  (X.catch
    (do
      val <- Iface.getSamplingStrategy handler (getSamplingStrategy_args_serviceName args)
      let res = default_GetSamplingStrategy_result{getSamplingStrategy_result_success = val}
      T.writeMessage oprot ("getSamplingStrategy", T.M_REPLY, seqid) $
        write_GetSamplingStrategy_result oprot res)
    ((\_ -> do
      T.writeMessage oprot ("getSamplingStrategy", T.M_EXCEPTION, seqid) $
        T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")) :: X.SomeException -> P.IO ()))
proc_ handler (iprot,oprot) (name,typ,seqid) = case name of
  "getSamplingStrategy" -> process_getSamplingStrategy (seqid,iprot,oprot,handler)
  _ -> do
    _ <- T.readVal iprot (T.T_STRUCT Map.empty)
    T.writeMessage oprot (name,T.M_EXCEPTION,seqid) $
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN_METHOD ("Unknown function " ++ LT.unpack name))
process handler (iprot, oprot) = do
  T.readMessage iprot (
    proc_ handler (iprot,oprot))
  P.return P.True
