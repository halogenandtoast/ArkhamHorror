module Arkham.Asset.Assets.JimCulver4Spec (spec) where

import Arkham.Ability (abilitySource)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes.HasGame (getGame)
import Arkham.Effect.Window (EffectWindow (EffectRoundWindow))
import Arkham.EffectMetadata (EffectMetadata (EffectModifiers))
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

-- Assign one point of incoming damage onto an Ally asset (soak).
chooseAssignDamageToAsset :: HasCallStack => AssetId -> TestAppT ()
chooseAssignDamageToAsset aid = chooseOptionMatching "assign damage to asset" \case
  AssetDamageLabel aid' _ -> aid' == aid
  _ -> False

-- Assign one point of incoming horror onto an Ally asset (soak).
chooseAssignHorrorToAsset :: HasCallStack => AssetId -> TestAppT ()
chooseAssignHorrorToAsset aid = chooseOptionMatching "assign horror to asset" \case
  AssetHorrorLabel aid' _ -> aid' == aid
  _ -> False

-- The sources of every ability currently on offer (empty when the game is not asking
-- anything).
offeredAbilitySources :: TestAppT [Source]
offeredAbilitySources = do
  questionMap <- gameQuestion <$> getGame
  let
    choices question = case stripQuestionWrappers question of
      ChooseOne msgs -> msgs
      PlayerWindowChooseOne msgs -> msgs
      _ -> []
  pure
    [ abilitySource ability
    | (_, question) <- mapToList questionMap
    , AbilityLabel {ability} <- choices question
    ]

spec :: Spec
spec = describe "Jim Culver (4)" do
  -- Regression for #5411. FAQ (2.12): an ability reacting to "you" taking damage/horror
  -- also covers assets you control, so an Ally soaking the entire hit still triggers it.
  it "triggers when all the damage is soaked by an Ally" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    withDeck self [Assets.flashlight]
    jimCulver <- self `putAssetIntoPlay` Assets.jimCulver4

    run $ InvestigatorAssignDamage self.id (TestSource mempty) DamageAny 1 0
    chooseAssignDamageToAsset jimCulver
    applyAllDamage

    useReactionOf jimCulver

    self.damage `shouldReturn` 0
    jimCulver.damage `shouldReturn` 1
    jimCulver.exhausted `shouldReturn` True
    fmap length self.hand `shouldReturn` 1
    self.resources `shouldReturn` 1

  it "triggers when all the horror is soaked by an Ally" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    withDeck self [Assets.flashlight]
    jimCulver <- self `putAssetIntoPlay` Assets.jimCulver4

    run $ InvestigatorAssignDamage self.id (TestSource mempty) DamageAny 0 1
    chooseAssignHorrorToAsset jimCulver
    applyAllHorror

    useReactionOf jimCulver

    self.horror `shouldReturn` 0
    jimCulver.horror `shouldReturn` 1
    jimCulver.exhausted `shouldReturn` True
    fmap length self.hand `shouldReturn` 1
    self.resources `shouldReturn` 1

  -- Regression for #5496. FAQ (2.12) covers horror dealt straight to an asset you
  -- control too (Field Agent (2) pays its cost that way), not just soaked horror.
  it "triggers when horror is dealt straight to an asset you control" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    withDeck self [Assets.flashlight]
    jimCulver <- self `putAssetIntoPlay` Assets.jimCulver4

    run $ DealAssetDamage jimCulver (TestSource mempty) 0 1

    useReactionOf jimCulver

    self.horror `shouldReturn` 0
    jimCulver.horror `shouldReturn` 1
    jimCulver.exhausted `shouldReturn` True
    fmap length self.hand `shouldReturn` 1
    self.resources `shouldReturn` 1

  it "triggers when the investigator takes the damage themselves" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    withDeck self [Assets.flashlight]
    jimCulver <- self `putAssetIntoPlay` Assets.jimCulver4

    run $ InvestigatorDirectDamage self.id (TestSource mempty) 1 0
    applyAllDamage

    useReactionOf jimCulver

    self.damage `shouldReturn` 1
    jimCulver.damage `shouldReturn` 0
    fmap length self.hand `shouldReturn` 1
    self.resources `shouldReturn` 1

  -- The other half of FAQ (2.12): points handed to another investigator (or their
  -- assets) are dealt to them, not to you, so the reaction must not be offered.
  it "does not trigger when all the damage is assigned to another investigator" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    other <- addInvestigator Investigators.rolandBanks
    other `moveTo` location
    withDeck self [Assets.flashlight]
    jimCulver <- self `putAssetIntoPlay` Assets.jimCulver4

    mods <- toModifiers (TestSource mempty) [CanAssignDamageToInvestigator other.id]
    run
      $ CreateWindowModifierEffect
        EffectRoundWindow
        (EffectModifiers mods)
        (TestSource mempty)
        (toTarget self)

    run $ InvestigatorAssignDamage self.id (TestSource mempty) DamageAny 1 0
    -- The single point goes to Roland, which resolves the whole assignment. No
    -- applyAllDamage afterwards: nothing asks again, and chooseOptionMatching leaves
    -- the answered question in place, so applyAllDamage would re-take the assignment
    -- and hand the point to Jenny.
    chooseOptionMatching "assign damage to the other investigator" \case
      DamageLabel iid _ -> iid == other.id
      _ -> False

    self.damage `shouldReturn` 0
    other.damage `shouldReturn` 1
    jimCulver.exhausted `shouldReturn` False
    -- Had the reaction fired, its window Ask would have replaced the stale assignment
    -- question with a WindowChooseOne carrying Jim Culver's AbilityLabel.
    offeredAbilitySources >>= \sources ->
      liftIO $ sources `shouldSatisfy` notElem (toSource jimCulver)
