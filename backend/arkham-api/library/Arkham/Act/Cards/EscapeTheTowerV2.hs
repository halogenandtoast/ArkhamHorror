module Arkham.Act.Cards.EscapeTheTowerV2 (escapeTheTowerV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Glyph, Omen))

newtype EscapeTheTowerV2 = EscapeTheTowerV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeTheTowerV2 :: ActCard EscapeTheTowerV2
escapeTheTowerV2 = act (2, A) EscapeTheTowerV2 Cards.escapeTheTowerV2 Nothing

instance HasAbilities EscapeTheTowerV2 where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (youExist $ at_ (locationIs Locations.eastAntechamber))
        $ ActionAbility #resign Nothing (ActionCost 1)
    , mkAbility a 2 $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
    , restricted a 3 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage EscapeTheTowerV2 where
  runMessage msg a@(EscapeTheTowerV2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- getPlayerCount
      discardTopOfEncounterDeckAndHandle iid (attrs.ability 2) (if n == 1 then 8 else 5) attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      let glyphs = filterCards (CardWithTrait Glyph) cards
      let omens = filterCards (CardWithTrait Omen) cards
      unless (null glyphs) $ focusCards glyphs do
        chooseTargetM iid glyphs $ drawCardFrom iid Deck.EncounterDiscard
      for_ omens $ drawCardFrom iid Deck.EncounterDiscard
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> EscapeTheTowerV2 <$> liftRunMessage msg attrs
