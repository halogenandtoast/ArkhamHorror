module Arkham.Investigator.Cards.TommyMuldoon (tommyMuldoon) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Field
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Helpers.Window (getDefeatedAsset)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (InvestigatorDamage)
import Arkham.Investigator.Types (Field (InvestigatorDamage, InvestigatorHorror))
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Projection

newtype TommyMuldoon = TommyMuldoon InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

tommyMuldoon :: InvestigatorCard TommyMuldoon
tommyMuldoon =
  investigator TommyMuldoon Cards.tommyMuldoon
    $ Stats {health = 8, sanity = 6, willpower = 3, intellect = 3, combat = 4, agility = 2}

instance HasAbilities TommyMuldoon where
  getAbilities (TommyMuldoon attrs) =
    [ restricted attrs 1 (Self <> youExist (oneOf [can.gain.resources, can.shuffle.deck]))
        $ freeReaction
        $ Matcher.AssetDefeated #when ByAny
        $ AssetControlledBy You
        <> oneOf [AssetWithHorror, AssetWithDamage]
    ]

instance HasChaosTokenValue TommyMuldoon where
  getChaosTokenValue iid ElderSign (TommyMuldoon attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage TommyMuldoon where
  runMessage msg i@(TommyMuldoon attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDefeatedAsset -> asset) _ -> do
      whenM (can.gain.resources iid) do
        damage <- field Field.AssetDamage asset
        horror <- field Field.AssetHorror asset
        hasBecky <- selectAny (assetIs Assets.becky)

        if hasBecky
          then do
            chooseAmounts
              iid
              ("Distribute " <> tshow (damage + horror) <> " Resources")
              (TotalAmountTarget $ damage + horror)
              [("Tommy Muldoon Resources", (0, damage + horror)), ("Becky Resources", (0, damage + horror))]
              (toTarget iid)
          else do
            gainResources iid (attrs.ability 1) (damage + horror)
      shuffleIntoDeck iid asset
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      hasDamage <- fieldSome InvestigatorDamage iid
      hasHorror <- fieldSome InvestigatorHorror iid
      assetsWithDamage <- selectAny (AssetWithDamage <> assetControlledBy iid)
      assetsWithHorror <- selectAny (AssetWithHorror <> assetControlledBy iid)
      assetsWithHealth <- selectAny (AssetWithHealth <> assetControlledBy iid)
      assetsWithSanity <- selectAny (AssetWithSanity <> assetControlledBy iid)

      chooseOrRunOne iid
        $ [ Label
              "Move up to 2 damage and/or horror from Tommy Muldoon to an asset you control"
              [HandleAbilityOption iid (toSource ElderSign) 1]
          | (hasDamage && assetsWithHealth) || (hasHorror && assetsWithSanity)
          ]
        <> [ Label
               "Move up to 2 damage and/or horror from an asset you control to Tommy Muldoon"
               [HandleAbilityOption iid (toSource ElderSign) 2]
           | assetsWithDamage || assetsWithHorror
           ]
        <> [Label "Do not move any damage and/or horror" []]
      pure i
    HandleAbilityOption (is attrs -> True) _ n | n `elem` [1, 11] -> do
      hasDamage <- fieldSome InvestigatorDamage attrs.id
      hasHorror <- fieldSome InvestigatorHorror attrs.id
      assetsWithHealth <- select (AssetCanBeAssignedDamageBy attrs.id <> assetControlledBy attrs.id)
      assetsWithSanity <- select (AssetCanBeAssignedHorrorBy attrs.id <> assetControlledBy attrs.id)

      when ((hasDamage && notNull assetsWithHealth) || (hasHorror && notNull assetsWithSanity)) $ do
        chooseOrRunOne attrs.id
          $ [Label "Done moving damage/horror" [] | n == 11]
          <> [ AssetHorrorLabel asset
                 $ MoveTokensNoDefeated #elderSign (toSource attrs.id) (toTarget asset) #horror 1
                 : [HandleAbilityOption attrs.id (toSource ElderSign) 11 | n == 1]
             | hasHorror
             , asset <- assetsWithSanity
             ]
          <> [ AssetDamageLabel asset
                 $ MoveTokensNoDefeated #elderSign (toSource attrs.id) (toTarget asset) #damage 1
                 : [HandleAbilityOption attrs.id (toSource ElderSign) 11 | n == 1]
             | hasDamage
             , asset <- assetsWithHealth
             ]

      when (n == 11) do
        assets <- select $ oneOf [AssetWithDamage, AssetWithHorror]
        for_ assets (checkDefeated ElderSign)
      pure i
    HandleAbilityOption (is attrs -> True) _ n | n `elem` [2, 22] -> do
      assetsWithDamage <- select (AssetWithDamage <> assetControlledBy attrs.id)
      assetsWithHorror <- select (AssetWithHorror <> assetControlledBy attrs.id)
      when (notNull assetsWithDamage || notNull assetsWithHorror) do
        chooseOrRunOne attrs.id
          $ [Label "Done moving damage/horror" [] | n == 22]
          <> [ AssetHorrorLabel asset
                 $ MoveTokensNoDefeated #elderSign (toSource asset) (toTarget attrs) #horror 1
                 : [HandleAbilityOption attrs.id (toSource ElderSign) 22 | n == 2]
             | asset <- assetsWithHorror
             ]
          <> [ AssetDamageLabel asset
                 $ MoveTokensNoDefeated #elderSign (toSource asset) (toTarget attrs) #damage 1
                 : [HandleAbilityOption attrs.id (toSource ElderSign) 22 | n == 2]
             | asset <- assetsWithDamage
             ]
      pure i
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let
        tommyResources = getChoiceAmount "Tommy Muldoon Resources" choices
        beckyUses = getChoiceAmount "Becky Resources" choices

      becky <- selectJust $ assetIs Assets.becky

      pushAll
        $ [TakeResources iid tommyResources (attrs.ability 1) False | tommyResources > 0]
        <> [AddUses #elderSign becky Ammo beckyUses | beckyUses > 0]
      pure i
    _ -> TommyMuldoon <$> liftRunMessage msg attrs
