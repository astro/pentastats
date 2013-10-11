{-# LANGUAGE RankNTypes #-}
module Aggregate where

import Data.Conduit
import Control.Monad.Trans


data Msg i = Input i
           | Stop
data Result i m a = Continue (Aggregate i m a)
                  | Result a
newtype Aggregate i m a = Aggregate { aggregate :: Msg i -> m (Result i m a) }

groupAggregate :: Monad m => (i -> i -> Bool) -> Aggregate i m i' -> Aggregate i' m a -> Aggregate i m a
groupAggregate cmpFun inner outer =
    groupAggregate' inner outer
    
  where groupAggregate' inner' outer' =
            Aggregate $ \msg ->
                case msg of
                  Input i ->
                      do Continue inner'' <- aggregate inner' $ Input i
                         return $ Continue $ Aggregate $ \msg' ->
                             case msg' of
                               Input i' | i `cmpFun` i' == False ->
                                          do Result a <- aggregate inner'' Stop
                                             Continue outer'' <- aggregate outer' $ Input a
                                             aggregate (groupAggregate' inner outer'') $ Input i'
                               _ ->
                                   aggregate (groupAggregate' inner'' outer') msg'
                  Stop ->
                      do Result a <- aggregate inner' Stop
                         Continue outer'' <- aggregate outer' $ Input a
                         Result a' <- aggregate outer'' Stop
                         return $ Result a'

foldAggregate :: Monad m => (st -> i -> m st) -> st -> (st -> m a) -> Aggregate i m a
foldAggregate f st fin =
    Aggregate $ \msg ->
    case msg of
      Input i ->
          do st' <- f st i
             return $ Continue $ foldAggregate f st' fin
      Stop ->
          do a <- fin st
             return $ Result a
             
aggregateSink :: Monad m => Aggregate i m a -> Sink i m a
aggregateSink agg =
    do mI <- await
       case mI of
         Just i ->
             do Continue agg' <- lift $ aggregate agg $ Input i
                aggregateSink agg'
         Nothing ->
             do Result a <- lift $ aggregate agg Stop
                return a
             