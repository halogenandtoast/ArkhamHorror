module Arkham.Event.Cards.Foresight1 (foresight1, Foresight1 (..)) where

import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (findAllCards)
import Arkham.Message qualified as Msg
import Arkham.Name

newtype Foresight1 = Foresight1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foresight1 :: EventCard Foresight1
foresight1 = event Foresight1 Cards.foresight1

instance RunMessage Foresight1 where
  runMessage msg e@(Foresight1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      cards <- findAllCards (const True)
      let cardNames = nub $ sort $ map toTitle cards
      chooseOneDropDown
        iid
        [ (name, Msg.createCardEffect Cards.foresight1 (Just $ EffectText name) attrs iid)
        | name <- cardNames
        ]
      pure e
    _ -> Foresight1 <$> lift (runMessage msg attrs)

newtype Foresight1Effect = Foresight1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foresight1Effect :: EffectArgs -> Foresight1Effect
foresight1Effect = cardEffectWith Foresight1Effect Cards.foresight1 (setEffectMeta False)

instance RunMessage Foresight1Effect where
  runMessage msg e@(Foresight1Effect attrs) = case msg of
    _ -> Foresight1Effect <$> runMessage msg attrs
