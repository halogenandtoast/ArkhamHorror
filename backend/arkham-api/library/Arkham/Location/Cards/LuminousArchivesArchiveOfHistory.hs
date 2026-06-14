module Arkham.Location.Cards.LuminousArchivesArchiveOfHistory (luminousArchivesArchiveOfHistory) where

import Arkham.Ability
import Arkham.Card (toCard)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Investigator.Types (Field (InvestigatorRemainingActions))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Scenarios.CourtOfTheAncients.Helpers
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
        restricted a 1 Here $ FastAbility (ActionCost 1)
      , -- [action] Move: move to a revealed Passageway location.
        restricted a 2 (Here <> exists revealedPassageway)
          $ ActionAbility #move Nothing (ActionCost 1)
      ]
   where
    revealedPassageway = RevealedLocation <> LocationWithTrait Passageway <> not_ (be a)

instance RunMessage LuminousArchivesArchiveOfHistory where
  runMessage msg l@(LuminousArchivesArchiveOfHistory attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- One action has already been spent as the ability cost (the minimum). Let the
      -- investigator spend additional actions (up to those remaining) to raise X.
      remaining <- field InvestigatorRemainingActions iid
      scenarioI18n $ chooseAmount' iid "additionalActions" "$actions" 0 remaining attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$actions" -> extra) (isTarget attrs -> True) -> do
      when (extra > 0) $ loseActions iid (attrs.ability 1) extra
      let x = 1 + extra
      -- Search the encounter discard pile for up to X Glyph cards and draw them.
      replicateM_ x $ findEncounterCardIn iid attrs (CardWithTrait Glyph) [FromEncounterDiscard]
      pure l
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) -> do
      drawCard iid card
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      passageways <- select $ RevealedLocation <> LocationWithTrait Passageway <> not_ (be attrs)
      chooseTargetM iid passageways $ moveTo (attrs.ability 2) iid
      pure l
    _ -> LuminousArchivesArchiveOfHistory <$> liftRunMessage msg attrs
