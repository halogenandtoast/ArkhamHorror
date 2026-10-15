module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Assets.RoderickUsher (roderickUsher) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Assets qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token (Token (Clue))

newtype RoderickUsher = RoderickUsher AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

roderickUsher :: AssetCard RoderickUsher
roderickUsher = asset RoderickUsher Cards.roderickUsher

instance HasAbilities RoderickUsher where
  getAbilities (RoderickUsher a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage RoderickUsher where
  runMessage msg a@(RoderickUsher attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \kind ->
          skillLabeled kind $ parley sid iid (attrs.ability 1) attrs kind (Fixed 2)
      pure a
    PassedThisSkillTestBy _ (isAbilitySource attrs 1 -> True) n | n > 0 -> do
      doStep n msg
      pure a
    DoStep n (PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) _) | n > 0 -> do
      iids <- select $ at_ (locationWithInvestigator iid) <> InvestigatorWithClues (atLeast 1)
      unless (null iids) do
        chooseOneM iid do
          targets iids \holder -> do
            moveTokens (attrs.ability 1) holder attrs Clue 1
            doNextStep msg
          withI18n $ labeled "doNotMoveClue" nothing
      pure a
    _ -> RoderickUsher <$> liftRunMessage msg attrs
