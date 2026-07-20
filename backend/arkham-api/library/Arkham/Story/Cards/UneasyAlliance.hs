module Arkham.Story.Cards.UneasyAlliance (uneasyAlliance) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Window (getScenarioEvent)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement (place)
import Arkham.Placement
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Scenarios.MachinationsThroughTime.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype UneasyAlliance = UneasyAlliance StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uneasyAlliance :: StoryCard UneasyAlliance
uneasyAlliance = story UneasyAlliance Cards.uneasyAlliance & persistStory

edwin :: AssetMatcher
edwin = assetIs Assets.edwinBennetAstuteAssociate

instance HasAbilities UneasyAlliance where
  getAbilities (UneasyAlliance a) =
    [ restricted a 1 (exists (edwin <> at_ YourLocation <> #ready)) parleyAction_
    , restricted a 2 (exists $ edwin <> oneOf [not_ (at_ YourLocation), not_ #ready]) actionAbility
    , mkAbility a 3 $ forced $ RoundEnds #when
    , mkAbility a 4 $ forced $ ScenarioEvent #when Nothing "edwinWouldBeAbducted"
    , onlyOnce
        $ restricted
          a
          5
          ( Remembered ThomasAndMaryHaveWonANobelPrize
              <> foldMap
                (notExists . storyIs)
                [Cards.anomaliesInSpacetime, Cards.mobTroubles, Cards.unspeakableAbomination]
          )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage UneasyAlliance where
  runMessage msg s@(UneasyAlliance attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withMatch edwin \edwin' -> do
        clues <- iid.clues
        when (clues > 0) $ moveTokens (attrs.ability 1) iid edwin' #clue 1
        sid <- getRandom
        chooseOneM iid $ withI18n do
          labeled' "skip" nothing
          scenarioI18n
            $ labeled' "uneasyAlliance.test"
            $ parley sid iid (attrs.ability 1) edwin' #intellect (Fixed 4)
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withMatch edwin \edwin' -> do
        clues <- iid.clues
        when (clues > 0) $ moveTokens (attrs.ability 1) iid edwin' #clue 1
      pure s
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      withMatch edwin exhaustThis
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withMatch edwin \edwin' -> do
        readyThis edwin'
        withLocationOf iid $ place edwin' . AtLocation
      pure s
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      selectOne edwin >>= \case
        Nothing -> placeDoomOnAgendaAndCheckAdvance 1
        Just edwin' -> do
          cluesOn <- field AssetClues edwin'
          if cluesOn > 0
            then removeTokens (attrs.ability 3) edwin' #clue 1
            else placeDoomOnAgendaAndCheckAdvance 1
      pure s
    UseCardAbility
      _
      (isSource attrs -> True)
      4
      ws@(getScenarioEvent @AssetId "edwinWouldBeAbducted" -> edwin')
      _ -> do
        cancelWindowBatch ws
        healAllDamageAndHorror (attrs.ability 4) edwin'
        agenda <- selectJust AnyAgenda
        placeDoom (attrs.ability 4) agenda 2
        push AdvanceAgendaIfThresholdSatisfied
        pure s
    UseThisAbility _ (isSource attrs -> True) 5 -> do
      lead <- getLead
      addToVictory lead attrs
      pure s
    FlipThis (isTarget attrs -> True) -> do
      flippedOver attrs
      pure $ UneasyAlliance $ attrs & flippedL .~ True
    _ -> UneasyAlliance <$> liftRunMessage msg attrs
