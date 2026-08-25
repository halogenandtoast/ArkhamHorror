module Arkham.Homebrew.CircusExMortis.Treacheries.CloseWatch (closeWatch) where

import Arkham.Ability
import Arkham.Card (card_)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype CloseWatch = CloseWatch TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeWatch :: TreacheryCard CloseWatch
closeWatch = treachery CloseWatch Cards.closeWatch

instance HasModifiersFor CloseWatch where
  getModifiersFor (CloseWatch a) = case a.placement of
    NextToAgenda ->
      modifySelect
        a
        Anywhere
        [ AdditionalCostToLeave
            $ OrCost
              [ ActionCost 1
              , FindEncounterCardCost (toTarget a) [FromEncounterDeck, FromEncounterDiscard] (card_ #enemy)
              ]
        ]
    _ -> pure mempty

instance HasAbilities CloseWatch where
  getAbilities (CloseWatch a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage CloseWatch where
  runMessage msg t@(CloseWatch attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      withLocationOf iid $ spawnEnemyAt_ card
      pure t
    _ -> CloseWatch <$> liftRunMessage msg attrs
