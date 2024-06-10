module Arkham.Event.Cards.Foresight1 (foresight1, foresight1Effect, Foresight1 (..)) where

import Arkham.Classes.HasGame
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (findAllCards)
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Message qualified as Msg
import Arkham.Name
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

-- Fast. Play when an investigator at your location would draw a card from their deck or from the encounter deck.
-- Name a card. If the drawn card is the named card, that investigator may either (choose one):
-- - Cancel that card's effects and discard it.
-- - Immediately play that card at -2 cost.

newtype Foresight1 = Foresight1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foresight1 :: EventCard Foresight1
foresight1 = event Foresight1 Cards.foresight1

allCardNames :: HasGame m => m [Text]
allCardNames = nub . sort . map toTitle <$> findAllCards (const True)

getWindowInvestigator :: [Window] -> InvestigatorId
getWindowInvestigator [] = error "getWindowInvestigator: empty list"
getWindowInvestigator ((windowType -> Window.WouldDrawCard iid _) : _) = iid
getWindowInvestigator (_ : ws) = getWindowInvestigator ws

instance RunMessage Foresight1 where
  runMessage msg e@(Foresight1 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ (getWindowInvestigator -> iid') _ | eid == toId attrs -> do
      cardNames <- allCardNames
      chooseOneDropDown
        iid
        [ (name, Msg.createCardEffect Cards.foresight1 (Just $ EffectText name) attrs iid')
        | name <- cardNames
        ]
      pure e
    _ -> Foresight1 <$> lift (runMessage msg attrs)

newtype Foresight1Effect = Foresight1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor Foresight1Effect where
  getModifiersFor target (Foresight1Effect a) | target == a.target = pure $ toModifiers a [Foresight]
  getModifiersFor _ _ = pure []

foresight1Effect :: EffectArgs -> Foresight1Effect
foresight1Effect = cardEffectWith Foresight1Effect Cards.foresight1 (setEffectMeta False)

instance RunMessage Foresight1Effect where
  runMessage msg e@(Foresight1Effect attrs) = case msg of
    _ -> Foresight1Effect <$> runMessage msg attrs
