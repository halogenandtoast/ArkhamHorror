module Arkham.Location.Cards.LuminousArchivesArchiveOfHistory (luminousArchivesArchiveOfHistory) where

import Arkham.Ability
import Arkham.Card (toCard)
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Glyph, Passageway))

newtype LuminousArchivesArchiveOfHistory = LuminousArchivesArchiveOfHistory LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luminousArchivesArchiveOfHistory :: LocationCard LuminousArchivesArchiveOfHistory
luminousArchivesArchiveOfHistory = location LuminousArchivesArchiveOfHistory Cards.luminousArchivesArchiveOfHistory 3 (Static 2)

instance HasModifiersFor LuminousArchivesArchiveOfHistory where
  getModifiersFor (LuminousArchivesArchiveOfHistory a) = do
    -- "Luminous Archives gets +1 shroud for each Glyph card at or attached to it."
    glyphAssets <- selectCount $ AssetWithTrait Glyph <> assetAt a
    glyphEnemies <- selectCount $ EnemyWithTrait Glyph <> enemyAt a
    glyphTreacheries <- selectCount $ TreacheryWithTrait Glyph <> TreacheryAttachedToLocation (be a)
    let n = glyphAssets + glyphEnemies + glyphTreacheries
    when (n > 0) $ modifySelf a [ShroudModifier n]

instance HasAbilities LuminousArchivesArchiveOfHistory where
  getAbilities (LuminousArchivesArchiveOfHistory a) =
    extendRevealed
      a
      [ -- [fast] Spend X actions (min 1): search the encounter discard pile for up
        -- to X Glyph cards and draw them one at a time. The 1-action minimum is the
        -- base FastAbility cost; additional actions (up to those remaining) are
        -- chosen on use.
        restricted a 1 (Here <> exists (InEncounterDiscard <> basic (CardWithTrait Glyph)))
          $ FastAbility (AtLeastOne (Fixed 1000) AdditionalActionCost)
      , -- [action] Move: move to a revealed Passageway location.
        restricted a 2 (Here <> exists revealedPassageway)
          $ ActionAbility #move Nothing (ActionCost 1)
      ]
   where
    revealedPassageway = RevealedLocation <> LocationWithTrait Passageway <> not_ (be a)

instance RunMessage LuminousArchivesArchiveOfHistory where
  runMessage msg l@(LuminousArchivesArchiveOfHistory attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalActionPayment -> n) -> do
      replicateM_ n $ findEncounterCardIn iid attrs (CardWithTrait Glyph) [FromEncounterDiscard]
      pure l
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) -> do
      -- Must name the source deck: every Glyph treachery branches on
      -- @drawnFrom == Just EncounterDiscard@ to decide whether it attaches to
      -- your location or surges and discards. Plain 'drawCard' records no deck,
      -- so the card the ability just fished out of the discard would surge.
      drawCardFrom iid Deck.EncounterDiscard card
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      passageways <- select $ RevealedLocation <> LocationWithTrait Passageway <> not_ (be attrs)
      chooseTargetM iid passageways $ moveTo (attrs.ability 2) iid
      pure l
    _ -> LuminousArchivesArchiveOfHistory <$> liftRunMessage msg attrs
