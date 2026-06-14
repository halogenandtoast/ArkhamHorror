module Arkham.Location.Cards.AerialWaterfall (aerialWaterfall) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Trait (Trait (Summit))

newtype AerialWaterfall = AerialWaterfall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aerialWaterfall :: LocationCard AerialWaterfall
aerialWaterfall = location AerialWaterfall Cards.aerialWaterfall 4 (Static 2)

instance HasAbilities AerialWaterfall where
  getAbilities (AerialWaterfall a) =
    extendRevealed
      a
      [ -- [fast] During your turn, spend 1 clue: choose any revealed Summit
        -- location and move to it. Limit once per round.
        playerLimit PerRound
          $ restricted a 1 (DuringTurn You)
          $ FastAbility (ClueCost $ Static 1)
      , -- [action] Spend 1 clue (per investigator): put the set-aside Obsidian
        -- Claw into play under any investigator's control (Speed side faceup).
        restricted a 2 Here
          $ actionAbilityWithCost (ClueCost $ PerPlayer 1)
      ]

instance RunMessage AerialWaterfall where
  runMessage msg l@(AerialWaterfall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <-
        select $ RevealedLocation <> LocationWithTrait Summit <> not_ (locationWithInvestigator iid)
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- The set-aside card def (11672a) is the "Speed" side faceup, so taking
      -- control of it puts the correct side into play.
      -- TODO: if a future ability needs to put the *Power* side (11672b) into
      -- play instead, swap to that card def / flip before taking control.
      claw <- getSetAsideCard Assets.obsidianClaw
      investigators <- getInvestigators
      chooseTargetM iid investigators \iid' -> takeControlOfSetAsideAsset iid' claw
      pure l
    _ -> AerialWaterfall <$> liftRunMessage msg attrs
