module Arkham.Homebrew.TheInvisibleMan.Assets.GriffinsNotes (griffinsNotes) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Import.Lifted hiding (PerformAction)
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Helpers.Investigator (guardAffectsOthers)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), hasModifier)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype GriffinsNotes = GriffinsNotes AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

griffinsNotes :: AssetCard GriffinsNotes
griffinsNotes = asset GriffinsNotes Cards.griffinsNotes

{- | "If this test is successful, look at the top card of any deck." The
investigation's test does not exist yet while the reaction resolves, so the
option arms a marker scoped to this investigator's next skill test and the peek
reads it back off that test.
-}
griffinsNotesPeek :: ModifierType
griffinsNotesPeek = ScenarioModifier "theInvisibleMan.griffinsNotesPeek"

instance HasAbilities GriffinsNotes where
  getAbilities (GriffinsNotes a) =
    [ scenarioI18n
        $ withI18nTooltip "griffinsNotes.study"
        $ reaction a 1 ControlsThis (assetUseCost a Secret 1) (PerformAction #when You #investigate)
    ]

instance RunMessage GriffinsNotes where
  runMessage msg a@(GriffinsNotes attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseNM iid 2 $ scenarioI18n $ scope "griffinsNotes" do
        -- The action's attack of opportunity is already queued by the time this
        -- window opens; whether the engaged enemies may make one is not.
        labeled "noAttacksOfOpportunity" do
          selectEach (enemyEngagedWith iid) \eid ->
            costModifier (attrs.ability 1) eid CannotMakeAttacksOfOpportunity
        labeled "shroud" do
          withLocationOf iid \lid ->
            thisSkillTestModifier iid (attrs.ability 1) lid (ShroudModifier (-2))
        labeled "additionalClue" do
          thisSkillTestModifier iid (attrs.ability 1) iid (DiscoveredClues 1)
        labeled "lookAtTopCard" do
          nextSkillTestModifier iid (attrs.ability 1) attrs griffinsNotesPeek
      pure a
    PassedSkillTest iid (Just Action.Investigate) _ _ _ _ | attrs.controller == Just iid -> do
      whenM (hasModifier attrs griffinsNotesPeek) do
        decks <- selectTargets =<< guardAffectsOthers iid can.manipulate.deck
        hasEncounterDeck <- can.target.encounterDeck iid
        chooseOneM iid do
          targets ([EncounterDeckTarget | hasEncounterDeck] <> decks) \target ->
            lookAt iid (attrs.ability 1) target [(FromTopOfDeck 1, PutBackInAnyOrder)] #any ReturnCards
      pure a
    _ -> GriffinsNotes <$> liftRunMessage msg attrs
