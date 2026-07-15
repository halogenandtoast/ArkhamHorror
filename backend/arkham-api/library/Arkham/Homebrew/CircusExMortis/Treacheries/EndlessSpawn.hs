module Arkham.Homebrew.CircusExMortis.Treacheries.EndlessSpawn (endlessSpawn) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Card
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Monster))
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EndlessSpawn = EndlessSpawn TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessSpawn :: TreacheryCard EndlessSpawn
endlessSpawn = treachery EndlessSpawn Cards.endlessSpawn

instance HasAbilities EndlessSpawn where
  getAbilities (EndlessSpawn a) = case a.attached.location of
    Just lid ->
      [ restricted a 1 (exists $ be lid <> LocationWithoutEnemies) $ forced $ RoundEnds #when
      , skillTestAbility $ restricted a 2 OnSameLocation actionAbility
      ]
    _ -> []

instance RunMessage EndlessSpawn where
  runMessage msg t@(EndlessSpawn attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findEncounterCard iid attrs (card_ $ #enemy <> withTrait Monster)
      pure t
    FoundEncounterCard _ (isTarget attrs -> True) (toCard -> card) -> do
      for_ attrs.attached.location $ createEnemyAt_ card
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      hasMoon <- selectAny $ chaosToken_ (ChaosTokenFaceIs MoonToken)
      chooseOneM iid do
        skillLabeled #agility $ beginSkillTest sid iid (attrs.ability 2) attrs #agility (Fixed 3)
        -- ponytail: "automatically succeed" modeled as a direct discard on the
        -- seal branch; outcome is identical to passing the test (discard Endless
        -- Spawn), and there is no clean primitive to force a specific test to pass.
        when hasMoon $ campaignI18n $ labeled' "endlessSpawn.autoSucceed" do
          selectOne (chaosToken_ (ChaosTokenFaceIs MoonToken)) >>= traverse_ (sealChaosToken iid iid)
          toDiscardBy iid (attrs.ability 2) attrs
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> EndlessSpawn <$> liftRunMessage msg attrs
