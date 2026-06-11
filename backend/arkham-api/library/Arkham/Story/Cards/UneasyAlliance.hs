module Arkham.Story.Cards.UneasyAlliance (uneasyAlliance) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.I18n
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
uneasyAlliance = storyWith UneasyAlliance Cards.uneasyAlliance (flippedL .~ True) & persistStory

edwin :: AssetMatcher
edwin = assetIs Assets.edwinBennetAstuteAssociate

instance HasAbilities UneasyAlliance where
  getAbilities (UneasyAlliance a) =
    [ restricted a 1 (exists (edwin <> AssetAt YourLocation <> AssetReady)) parleyAction_
    , restricted a 2 (exists edwin) actionAbility
    , mkAbility a 3 $ forced $ RoundEnds #when
    , restricted
        a
        4
        ( Remembered ThomasAndMaryHaveWonANobelPrize
            <> notExists
              ( mapOneOf
                  storyIs
                  [Cards.anomaliesInSpacetime, Cards.mobTroubles, Cards.unspeakableAbomination]
              )
        )
        (forced AnyWindow)
    ]

instance RunMessage UneasyAlliance where
  runMessage msg s@(UneasyAlliance attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectForMaybeM (edwin <> AssetAt (locationWithInvestigator iid)) \edwin' -> do
        clues <- iid.clues
        when (clues > 0) do
          push $ RemoveClues (attrs.ability 1) (toTarget iid) 1
          placeTokens (attrs.ability 1) edwin' #clue 1
        sid <- getRandom
        chooseOneM iid $ withI18n do
          labeled' "skip" nothing
          scenarioI18n
            $ labeled' "uneasyAlliance.test"
            $ parley sid iid (attrs.ability 1) edwin' #intellect (Fixed 4)
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      selectForMaybeM edwin \edwin' -> do
        clues <- iid.clues
        when (clues > 0) do
          push $ RemoveClues (attrs.ability 1) (toTarget iid) 1
          placeTokens (attrs.ability 1) edwin' #clue 1
      pure s
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      selectForMaybeM edwin \edwin' -> exhaustThis edwin'
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      selectForMaybeM edwin \edwin' -> do
        readyThis edwin'
        getLocationOf iid >>= traverse_ \lid -> place edwin' (AtLocation lid)
      pure s
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      mEdwin <- selectOne edwin
      cluesOn <- maybe (pure 0) (field AssetClues) mEdwin
      if cluesOn > 0
        then for_ mEdwin \edwin' -> removeTokens (attrs.ability 3) edwin' #clue 1
        else do
          agenda <- selectJust AnyAgenda
          placeDoom (attrs.ability 3) agenda 1
          push AdvanceAgendaIfThresholdSatisfied
      pure s
    UseThisAbility _ (isSource attrs -> True) 4 -> do
      lead <- getLead
      addToVictory lead attrs
      pure s
    _ -> UneasyAlliance <$> liftRunMessage msg attrs
