module Arkham.Location.Cards.WeedChokedBeach (weedChokedBeach) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype WeedChokedBeach = WeedChokedBeach LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

weedChokedBeach :: LocationCard WeedChokedBeach
weedChokedBeach = locationWith WeedChokedBeach Cards.weedChokedBeach 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities WeedChokedBeach where
  getAbilities (WeedChokedBeach attrs) =
    extendRevealed
      attrs
      [ mkAbility attrs 1 $ forced $ SkillTestResult #after You WhileEvading #failure
      , mkAbility attrs 2 $ forced $ Leaves #when You (be attrs)
      ]

instance RunMessage WeedChokedBeach where
  runMessage msg l@(WeedChokedBeach attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseActions iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      requestChaosTokens iid (attrs.ability 2) 1
      pure l
    RequestedChaosTokens (isAbilitySource attrs 2 -> True) (Just iid) (map (.face) -> tokens) -> do
      withI18n $ prompt_ iid "continue"
      let triggeringTokens = [#skull, #tablet, #elderthing, #autofail]
      when (any (`elem` triggeringTokens) tokens) do
        hasItems <- selectAny $ DiscardableAsset <> #item <> assetControlledBy iid
        if hasItems
          then chooseOneM iid $ withI18n do
            labeled' "cancelMove" $ cancelMovement (attrs.ability 2) iid
            labeled' "discardAssets" $ chooseAndDiscardAssetMatching iid (attrs.ability 2) #item
          else cancelMovement (attrs.ability 2) iid
      resetChaosTokens (attrs.ability 2)
      pure l
    _ -> WeedChokedBeach <$> liftRunMessage msg attrs
