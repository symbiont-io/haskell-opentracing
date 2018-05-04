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

module AggregationValidator where
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


import Aggregation_validator_Types
import qualified AggregationValidator_Iface as Iface
-- HELPER FUNCTIONS AND STRUCTURES --

data ValidateTrace_args = ValidateTrace_args  { validateTrace_args_traceId :: LT.Text
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable ValidateTrace_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` validateTrace_args_traceId record  
instance QC.Arbitrary ValidateTrace_args where 
  arbitrary = M.liftM ValidateTrace_args (QC.arbitrary)
  shrink obj | obj == default_ValidateTrace_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_ValidateTrace_args{validateTrace_args_traceId = validateTrace_args_traceId obj} then P.Nothing else P.Just $ default_ValidateTrace_args{validateTrace_args_traceId = validateTrace_args_traceId obj}
    ]
from_ValidateTrace_args :: ValidateTrace_args -> T.ThriftVal
from_ValidateTrace_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v9 -> P.Just (1, ("traceId",T.TString $ E.encodeUtf8 _v9))) $ validateTrace_args_traceId record
  ]
write_ValidateTrace_args :: T.Protocol p => p -> ValidateTrace_args -> P.IO ()
write_ValidateTrace_args oprot record = T.writeVal oprot $ from_ValidateTrace_args record
encode_ValidateTrace_args :: T.StatelessProtocol p => p -> ValidateTrace_args -> LBS.ByteString
encode_ValidateTrace_args oprot record = T.serializeVal oprot $ from_ValidateTrace_args record
to_ValidateTrace_args :: T.ThriftVal -> ValidateTrace_args
to_ValidateTrace_args (T.TStruct fields) = ValidateTrace_args{
  validateTrace_args_traceId = P.maybe (P.error "Missing required field: traceId") (\(_,_val11) -> (case _val11 of {T.TString _val12 -> E.decodeUtf8 _val12; _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_ValidateTrace_args _ = P.error "not a struct"
read_ValidateTrace_args :: T.Protocol p => p -> P.IO ValidateTrace_args
read_ValidateTrace_args iprot = to_ValidateTrace_args <$> T.readVal iprot (T.T_STRUCT typemap_ValidateTrace_args)
decode_ValidateTrace_args :: T.StatelessProtocol p => p -> LBS.ByteString -> ValidateTrace_args
decode_ValidateTrace_args iprot bs = to_ValidateTrace_args $ T.deserializeVal iprot (T.T_STRUCT typemap_ValidateTrace_args) bs
typemap_ValidateTrace_args :: T.TypeMap
typemap_ValidateTrace_args = Map.fromList [(1,("traceId",T.T_STRING))]
default_ValidateTrace_args :: ValidateTrace_args
default_ValidateTrace_args = ValidateTrace_args{
  validateTrace_args_traceId = ""}
data ValidateTrace_result = ValidateTrace_result  { validateTrace_result_success :: ValidateTraceResponse
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable ValidateTrace_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` validateTrace_result_success record  
instance QC.Arbitrary ValidateTrace_result where 
  arbitrary = M.liftM ValidateTrace_result (QC.arbitrary)
  shrink obj | obj == default_ValidateTrace_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_ValidateTrace_result{validateTrace_result_success = validateTrace_result_success obj} then P.Nothing else P.Just $ default_ValidateTrace_result{validateTrace_result_success = validateTrace_result_success obj}
    ]
from_ValidateTrace_result :: ValidateTrace_result -> T.ThriftVal
from_ValidateTrace_result record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v15 -> P.Just (0, ("success",from_ValidateTraceResponse _v15))) $ validateTrace_result_success record
  ]
write_ValidateTrace_result :: T.Protocol p => p -> ValidateTrace_result -> P.IO ()
write_ValidateTrace_result oprot record = T.writeVal oprot $ from_ValidateTrace_result record
encode_ValidateTrace_result :: T.StatelessProtocol p => p -> ValidateTrace_result -> LBS.ByteString
encode_ValidateTrace_result oprot record = T.serializeVal oprot $ from_ValidateTrace_result record
to_ValidateTrace_result :: T.ThriftVal -> ValidateTrace_result
to_ValidateTrace_result (T.TStruct fields) = ValidateTrace_result{
  validateTrace_result_success = P.maybe (validateTrace_result_success default_ValidateTrace_result) (\(_,_val17) -> (case _val17 of {T.TStruct _val18 -> (to_ValidateTraceResponse (T.TStruct _val18)); _ -> P.error "wrong type"})) (Map.lookup (0) fields)
  }
to_ValidateTrace_result _ = P.error "not a struct"
read_ValidateTrace_result :: T.Protocol p => p -> P.IO ValidateTrace_result
read_ValidateTrace_result iprot = to_ValidateTrace_result <$> T.readVal iprot (T.T_STRUCT typemap_ValidateTrace_result)
decode_ValidateTrace_result :: T.StatelessProtocol p => p -> LBS.ByteString -> ValidateTrace_result
decode_ValidateTrace_result iprot bs = to_ValidateTrace_result $ T.deserializeVal iprot (T.T_STRUCT typemap_ValidateTrace_result) bs
typemap_ValidateTrace_result :: T.TypeMap
typemap_ValidateTrace_result = Map.fromList [(0,("success",(T.T_STRUCT typemap_ValidateTraceResponse)))]
default_ValidateTrace_result :: ValidateTrace_result
default_ValidateTrace_result = ValidateTrace_result{
  validateTrace_result_success = default_ValidateTraceResponse}
process_validateTrace (seqid, iprot, oprot, handler) = do
  args <- read_ValidateTrace_args iprot
  (X.catch
    (do
      val <- Iface.validateTrace handler (validateTrace_args_traceId args)
      let res = default_ValidateTrace_result{validateTrace_result_success = val}
      T.writeMessage oprot ("validateTrace", T.M_REPLY, seqid) $
        write_ValidateTrace_result oprot res)
    ((\_ -> do
      T.writeMessage oprot ("validateTrace", T.M_EXCEPTION, seqid) $
        T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")) :: X.SomeException -> P.IO ()))
proc_ handler (iprot,oprot) (name,typ,seqid) = case name of
  "validateTrace" -> process_validateTrace (seqid,iprot,oprot,handler)
  _ -> do
    _ <- T.readVal iprot (T.T_STRUCT Map.empty)
    T.writeMessage oprot (name,T.M_EXCEPTION,seqid) $
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN_METHOD ("Unknown function " ++ LT.unpack name))
process handler (iprot, oprot) = do
  T.readMessage iprot (
    proc_ handler (iprot,oprot))
  P.return P.True