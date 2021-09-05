module Arkham.Types.Effect.Effects.SleightOfHand
  ( SleightOfHand(..)
  , sleightOfHand
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype SleightOfHand = SleightOfHand EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleightOfHand :: EffectArgs -> SleightOfHand
sleightOfHand = SleightOfHand . uncurry4 (baseAttrs "03029")

instance HasModifiersFor env SleightOfHand

instance
  ( HasId (Maybe OwnerId) env AssetId
  , Query AssetMatcher env
  , HasQueue env
  )
  => RunMessage env SleightOfHand where
  runMessage msg e@(SleightOfHand attrs) = case msg of
    EndTurn _ -> do
      case effectTarget attrs of
        AssetTarget aid -> do
          inPlay <- isJust <$> selectOne (AssetWithId aid)
          when inPlay $ do
            mOwner :: Maybe OwnerId <- getId aid
            for_ mOwner $ \ownerId ->
              push (ReturnToHand (unOwnerId ownerId) (AssetTarget aid))
        _ -> pure ()
      e <$ push (Discard $ toTarget attrs)
    _ -> SleightOfHand <$> runMessage msg attrs
