module Arkham.Location.Cards.TheDrownedCity.CourtOfTheAncients.TwistingCatwalks (twistingCatwalks) where

import Arkham.Ability
import Arkham.Card (filterCards)
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Enemy (getDefeatedEnemyHealth)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Helpers.Window (defeatedEnemy)
import Arkham.Location.CardDefs.TheDrownedCity.CourtOfTheAncients qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Glyph))

newtype TwistingCatwalks = TwistingCatwalks LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistingCatwalks :: LocationCard TwistingCatwalks
twistingCatwalks = location TwistingCatwalks Cards.twistingCatwalks 3 (PerPlayer 1)

instance HasAbilities TwistingCatwalks where
  getAbilities (TwistingCatwalks a) =
    extendRevealed
      a
      [ restricted a 1 Here
          $ forced
          $ DealtDamageOrHorror #after (SourceIsEnemy $ at_ (be a)) You
      , restricted a 2 Here
          $ freeReaction (IfEnemyDefeated #after You ByAny (EnemyWasAt $ be a))
      ]

instance RunMessage TwistingCatwalks where
  runMessage msg l@(TwistingCatwalks attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCard iid (attrs.ability 1)
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 (defeatedEnemy -> eid) _ -> do
      getDefeatedEnemyHealth eid >>= traverse_ \health ->
        when (health > 0) $ discardTopOfEncounterDeckAndHandle iid (attrs.ability 2) health attrs
      pure l
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      -- Only 1 Glyph. Named deck, not plain 'drawCard': Glyph treacheries only
      -- attach when @drawnFrom == Just EncounterDiscard@, otherwise they surge.
      let glyphs = filterCards (CardWithTrait Glyph) cards
      unless (null glyphs) $ focusCards glyphs do
        chooseTargetM iid glyphs $ drawCardFrom iid Deck.EncounterDiscard
      pure l
    _ -> TwistingCatwalks <$> liftRunMessage msg attrs
