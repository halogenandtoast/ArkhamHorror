module Arkham.UltimatumsAndBoons.UltimatumOfAgonySpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes.HasGame (getGame)
import Helpers.UltimatumsAndBoons
import TestImport.New

-- | The single pending damage-assignment question's choices, with the
-- display-only wrappers peeled off.
pendingChoices :: TestAppT [UI Message]
pendingChoices = do
  questionMap <- gameQuestion <$> getGame
  case mapToList questionMap of
    [(_, q)] -> case stripQuestionWrappers q of
      ChooseOne msgs -> pure msgs
      _ -> pure []
    _ -> pure []

isInvestigatorDamageChoice :: UI Message -> Bool
isInvestigatorDamageChoice = \case
  ComponentLabel (InvestigatorComponent _ DamageToken) _ -> True
  _ -> False

isAssetDamageChoice :: UI Message -> Bool
isAssetDamageChoice = \case
  ComponentLabel (AssetComponent _ DamageToken) _ -> True
  _ -> False

spec :: Spec
spec = describe "Ultimatum of Agony" $ do
  it "forces damage onto the most recent card until its health is used up" . gameTest $ \self -> do
    withUltimatums [UltimatumOfAgony]
    dog <- self `putAssetIntoPlay` Assets.guardDog
    run $ assignDamage (toId self) (TestSource mempty) 4

    -- the first point may be assigned freely
    choices1 <- pendingChoices
    choices1 `shouldSatisfy` any isInvestigatorDamageChoice
    choices1 `shouldSatisfy` any isAssetDamageChoice
    assignDamageTo self (AssetSource dog)

    -- while the dog (3 health) can still absorb damage, it is the only choice
    choices2 <- pendingChoices
    choices2 `shouldSatisfy` notNull
    choices2 `shouldSatisfy` all isAssetDamageChoice
    assignDamageTo self (AssetSource dog)

    choices3 <- pendingChoices
    choices3 `shouldSatisfy` notNull
    choices3 `shouldSatisfy` all isAssetDamageChoice
    assignDamageTo self (AssetSource dog)

    -- the dog is full, so the excess may go elsewhere
    choices4 <- pendingChoices
    choices4 `shouldSatisfy` any isInvestigatorDamageChoice
    choices4 `shouldSatisfy` (not . any isAssetDamageChoice)
    applyAllDamage
    self.damage `shouldReturn` 1

  it "has no effect while ultimatums and boons are disabled" . gameTest $ \self -> do
    withUltimatumsDisabled [UltimatumOfAgony]
    dog <- self `putAssetIntoPlay` Assets.guardDog
    run $ assignDamage (toId self) (TestSource mempty) 4
    assignDamageTo self (AssetSource dog)
    -- the second point may still be assigned freely
    choices <- pendingChoices
    choices `shouldSatisfy` any isInvestigatorDamageChoice
    choices `shouldSatisfy` any isAssetDamageChoice
    applyAllDamage
