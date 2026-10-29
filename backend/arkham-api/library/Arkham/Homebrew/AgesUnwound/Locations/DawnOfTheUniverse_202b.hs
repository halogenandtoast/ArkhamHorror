module Arkham.Homebrew.AgesUnwound.Locations.DawnOfTheUniverse_202b (dawnOfTheUniverse_202b) where

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetStartingUses, AssetUses))
import Arkham.Helpers.Use (toStartingUses)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers (timeRunsOutI18n)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype DawnOfTheUniverse_202b = DawnOfTheUniverse_202b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dawnOfTheUniverse_202b :: LocationCard DawnOfTheUniverse_202b
dawnOfTheUniverse_202b =
  symbolLabel $ location DawnOfTheUniverse_202b Cards.dawnOfTheUniverse_202b 6 (PerPlayer 1)

instance HasAbilities DawnOfTheUniverse_202b where
  getAbilities (DawnOfTheUniverse_202b a) =
    extendRevealed
      a
      [ restricted
          a
          1
          (exists $ You <> HasMatchingAsset (AssetControlledBy You <> AssetCanLeavePlayByNormalMeans))
          $ forced
          $ SkillTestResult #after You (WhileInvestigating (be a)) #failure
      , restricted a 2 (thisExists a LocationWithAnyClues) $ forced $ RoundEnds #when
      ]

instance RunMessage DawnOfTheUniverse_202b where
  runMessage msg l@(DawnOfTheUniverse_202b attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> AssetCanLeavePlayByNormalMeans
      chooseTargetM iid assets \aid -> do
        starting <- fieldMapM AssetStartingUses toStartingUses aid
        remaining <- field AssetUses aid
        let spent = sum starting - sum remaining
        returnToHand iid aid
        when (spent > 0) do
          chooseOneM iid $ timeRunsOutI18n $ scope "dawnOfTheUniverse" do
            countVar spent $ labeled "takeDamage" $ assignDamage iid (attrs.ability 1) spent
            countVar spent $ labeled "takeHorror" $ assignHorror iid (attrs.ability 1) spent
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      placeDoomOnAgenda 1
      pure l
    _ -> DawnOfTheUniverse_202b <$> liftRunMessage msg attrs
