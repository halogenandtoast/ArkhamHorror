module Arkham.Investigator.Cards.TommyMuldoon (
  tommyMuldoon,
  TommyMuldoon (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Field
import Arkham.Asset.Uses
import Arkham.Deck qualified as Deck
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Projection
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype TommyMuldoon = TommyMuldoon InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock (Data)

tommyMuldoon :: InvestigatorCard TommyMuldoon
tommyMuldoon =
  investigator TommyMuldoon Cards.tommyMuldoon
    $ Stats {health = 8, sanity = 6, willpower = 3, intellect = 3, combat = 4, agility = 2}

instance HasAbilities TommyMuldoon where
  getAbilities (TommyMuldoon attrs) =
    [ restrictedAbility attrs 1 (Self <> CanGainResources)
        $ freeReaction
        $ Matcher.AssetDefeated #when ByAny
        $ AssetControlledBy You
        <> oneOf [AssetWithHorror, AssetWithDamage]
    ]

instance HasChaosTokenValue TommyMuldoon where
  getChaosTokenValue iid ElderSign (TommyMuldoon attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

getAsset :: [Window] -> AssetId
getAsset = \case
  ((windowType -> Window.AssetDefeated aid _) : _) -> aid
  (_ : rest) -> getAsset rest
  _ -> error "impossible"

instance RunMessage TommyMuldoon where
  runMessage msg i@(TommyMuldoon attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getAsset -> asset) _ -> do
      damage <- field Field.AssetDamage asset
      horror <- field Field.AssetHorror asset

      hasBecky <- selectAny (assetIs Assets.becky)
      player <- getPlayer iid

      pushAll
        $ if hasBecky
          then
            [ chooseAmounts
                player
                ("Distribute " <> tshow (damage + horror) <> " Resources")
                (TotalAmountTarget $ damage + horror)
                [("Tommy Muldoon Resources", (0, damage + horror)), ("Becky Resources", (0, damage + horror))]
                (toTarget iid)
            ]
          else
            [TakeResources iid (damage + horror) (toAbilitySource attrs 1) False]
              <> [ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget asset)]
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      hasDamage <- fieldSome InvestigatorDamage iid
      hasHorror <- fieldSome InvestigatorHorror iid
      assetsWithDamage <- selectAny (AssetWithDamage <> assetControlledBy iid)
      assetsWithHorror <- selectAny (AssetWithHorror <> assetControlledBy iid)
      assetsWithHealth <- selectAny (AssetWithHealth <> assetControlledBy iid)
      assetsWithSanity <- selectAny (AssetWithSanity <> assetControlledBy iid)
      player <- getPlayer iid

      push
        $ chooseOrRunOne player
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
    HandleAbilityOption iid _ n
      | attrs `is` iid
      , n `elem` [1, 11] -> do
          hasDamage <- fieldSome InvestigatorDamage iid
          hasHorror <- fieldSome InvestigatorHorror iid
          assetsWithHealth <- select (AssetWithHealth <> assetControlledBy iid)
          assetsWithSanity <- select (AssetWithSanity <> assetControlledBy iid)
          player <- getPlayer iid

          when ((hasDamage && notNull assetsWithHealth) || (hasHorror && notNull assetsWithSanity)) $ do
            push
              $ chooseOrRunOne player
              $ [Label "Done moving damage/horror" [] | n == 11]
              <> [ AssetHorrorLabel asset
                  $ MovedHorror #elderSign (toSource iid) (toTarget asset) 1
                  : [HandleAbilityOption iid (toSource ElderSign) 11 | n == 1]
                 | asset <- assetsWithSanity
                 ]
              <> [ AssetDamageLabel asset
                  $ MovedDamage #elderSign (toSource iid) (toTarget asset) 1
                  : [HandleAbilityOption iid (toSource ElderSign) 11 | n == 1]
                 | asset <- assetsWithHealth
                 ]

          pure i
    HandleAbilityOption iid _ n
      | attrs `is` iid
      , n `elem` [2, 22] -> do
          assetsWithDamage <- select (AssetWithDamage <> assetControlledBy iid)
          assetsWithHorror <- select (AssetWithHorror <> assetControlledBy iid)
          player <- getPlayer iid
          pushWhen (notNull assetsWithDamage || notNull assetsWithHorror)
            $ chooseOrRunOne player
            $ [Label "Done moving damage/horror" [] | n == 22]
            <> [ AssetHorrorLabel asset
                $ MovedHorror #elderSign (toSource asset) (toTarget iid) 1
                : [HandleAbilityOption iid (toSource ElderSign) 22 | n == 2]
               | asset <- assetsWithHorror
               ]
            <> [ AssetDamageLabel asset
                $ MovedDamage #elderSign (toSource asset) (toTarget iid) 1
                : [HandleAbilityOption iid (toSource ElderSign) 22 | n == 2]
               | asset <- assetsWithDamage
               ]
          pure i
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let
        tommyResources = getChoiceAmount "Tommy Muldoon Resources" choices
        beckyUses = getChoiceAmount "Becky Resources" choices

      becky <- selectJust $ assetIs Assets.becky

      pushAll
        $ [TakeResources iid tommyResources (toAbilitySource attrs 1) False | tommyResources > 0]
        <> [AddUses #elderSign becky Ammo beckyUses | beckyUses > 0]
      pure i
    _ -> TommyMuldoon <$> runMessage msg attrs
