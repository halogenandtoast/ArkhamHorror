module Arkham.Effect.Effects.Mesmerize (
  Mesmerize (..),
  mesmerize,
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Matcher

newtype Mesmerize = Mesmerize EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mesmerize :: EffectArgs -> Mesmerize
mesmerize = Mesmerize . uncurry4 (baseAttrs "82035")

instance RunMessage Mesmerize where
  runMessage msg e@(Mesmerize attrs) = case msg of
    Flipped _ card -> do
      if toCardDef card == Assets.innocentReveler
        then do
          aid <- selectJust $ AssetWithCardId $ toCardId card
          case effectTarget attrs of
            InvestigatorTarget iid -> do
              locationTargets <-
                selectListMap LocationTarget $ FarthestLocationFromYou LocationWithoutInvestigators
              e
                <$ pushAll
                  [ chooseOne
                      iid
                      [ TargetLabel locationTarget [AttachAsset aid locationTarget]
                      | locationTarget <- locationTargets
                      ]
                  , AssetDamage aid (effectSource attrs) 1 1
                  , DisableEffect $ toId attrs
                  ]
            _ -> error "Must be investigator target"
        else e <$ push (DisableEffect $ toId attrs)
    _ -> Mesmerize <$> runMessage msg attrs
