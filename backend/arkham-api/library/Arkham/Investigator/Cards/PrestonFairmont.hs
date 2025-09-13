module Arkham.Investigator.Cards.PrestonFairmont (prestonFairmont) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.Cost
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype PrestonFairmont = PrestonFairmont InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

prestonFairmont :: InvestigatorCard PrestonFairmont
prestonFairmont =
  investigator PrestonFairmont Cards.prestonFairmont
    $ Stats {health = 7, sanity = 7, willpower = 1, intellect = 1, combat = 1, agility = 1}

instance HasChaosTokenValue PrestonFairmont where
  getChaosTokenValue iid ElderSign (PrestonFairmont attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 0
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage PrestonFairmont where
  runMessage msg i@(PrestonFairmont attrs) = case msg of
    TakeResources iid n source False | iid == toId attrs -> do
      mFamilyInheritance <- selectOne $ assetIs Assets.familyInheritance
      case mFamilyInheritance of
        Nothing -> pure i
        Just familyInheritance -> do
          canGainResources <- not <$> hasModifier attrs CannotGainResources
          case source of
            InvestigatorSource iid' | iid == iid' -> PrestonFairmont <$> runMessage msg attrs
            ResourceSource iid' | iid == iid' -> PrestonFairmont <$> runMessage msg attrs
            AbilitySource abilitySource _ -> do
              if abilitySource == AssetSource familyInheritance
                then PrestonFairmont <$> runMessage msg attrs
                else do
                  pushWhen canGainResources $ PlaceResources (toSource attrs) (toTarget familyInheritance) n
                  pure i
            UseAbilitySource _ abilitySource _ -> do
              if abilitySource == AssetSource familyInheritance
                then PrestonFairmont <$> runMessage msg attrs
                else do
                  pushWhen canGainResources $ PlaceResources (toSource attrs) (toTarget familyInheritance) n
                  pure i
            _ -> do
              pushWhen canGainResources $ PlaceResources (toSource attrs) (toTarget familyInheritance) n
              pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      hasResources <- (>= 2) <$> getSpendableResources iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label "Resolve normally" []
        : [Label "Automatically succeed" [SpendResources iid 2, PassSkillTest] | hasResources]
      pure i
    Blanked msg'@(SpendResources iid _) | iid == toId attrs -> runMessage msg' i
    SpendResources iid n | iid == toId attrs -> do
      mFamilyInheritance <- selectOne $ assetIs Assets.familyInheritance
      case mFamilyInheritance of
        Nothing -> PrestonFairmont <$> runMessage msg attrs
        Just familyInheritance -> do
          familyInheritanceResources <- field AssetResources familyInheritance
          if familyInheritanceResources > 0
            then do
              if attrs.resources > 0
                then do
                  player <- getPlayer iid
                  push
                    $ chooseOrRunN player n
                    $ replicate
                      familyInheritanceResources
                      (targetLabel familyInheritance [RemoveResources (toSource attrs) (toTarget familyInheritance) 1])
                    <> replicate attrs.resources (ResourceLabel iid [Do (SpendResources iid 1)])
                else push $ RemoveResources (toSource attrs) (toTarget familyInheritance) n
              pure i
            else PrestonFairmont <$> runMessage msg attrs
    _ -> PrestonFairmont <$> runMessage msg attrs
