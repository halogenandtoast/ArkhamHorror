module Arkham.Treachery.Cards.Mesmerize (
  mesmerize,
  mesmerizeEffect,
  Mesmerize (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Mesmerize = Mesmerize TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mesmerize :: TreacheryCard Mesmerize
mesmerize = treachery Mesmerize Cards.mesmerize

instance RunMessage Mesmerize where
  runMessage msg t@(Mesmerize attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mlid <- field InvestigatorLocation iid
      case mlid of
        Nothing -> push $ gainSurge attrs
        Just lid -> do
          maskedCarnevaleGoers <- selectTargets (at_ (be lid) <> AssetWithTitle "Masked Carnevale-Goer")
          player <- getPlayer iid
          case maskedCarnevaleGoers of
            [] -> push $ chooseOne player [Label "Surge" [gainSurge attrs]]
            xs ->
              pushAll
                [ createCardEffect Cards.mesmerize Nothing attrs iid
                , chooseOne player $ targetLabels xs (only . Flip iid (toSource attrs))
                ]
      pure t
    _ -> Mesmerize <$> runMessage msg attrs

newtype MesmerizeEffect = MesmerizeEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mesmerizeEffect :: EffectArgs -> MesmerizeEffect
mesmerizeEffect = cardEffect MesmerizeEffect Cards.mesmerize

instance RunMessage MesmerizeEffect where
  runMessage msg e@(MesmerizeEffect attrs) = case msg of
    Flipped _ card -> do
      if toCardDef card == Assets.innocentReveler
        then do
          aid <- selectJust $ AssetWithCardId $ toCardId card
          case effectTarget attrs of
            InvestigatorTarget iid -> do
              locationTargets <- selectTargets $ FarthestLocationFromYou LocationWithoutInvestigators
              player <- getPlayer iid
              pushAll
                [ chooseOne player $ targetLabels locationTargets (only . AttachAsset aid)
                , AssetDamage aid (effectSource attrs) 1 1
                , disable attrs
                ]
            _ -> error "Must be investigator target"
        else push $ disable attrs
      pure e
    _ -> MesmerizeEffect <$> runMessage msg attrs
