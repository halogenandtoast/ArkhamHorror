module Arkham.Homebrew.AgainstTheWendigo.Stories.CharlieFoxtailsDestiny (charlieFoxtailsDestiny) where

import Arkham.Ability
import Arkham.Card (toCard)
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (scenarioI18n)
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Story.Import.Lifted
import Arkham.Token (Token (Damage))

newtype CharlieFoxtailsDestiny = CharlieFoxtailsDestiny StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlieFoxtailsDestiny :: StoryCard CharlieFoxtailsDestiny
charlieFoxtailsDestiny = persistStory $ story CharlieFoxtailsDestiny Cards.charlieFoxtailsDestiny

instance HasAbilities CharlieFoxtailsDestiny where
  getAbilities (CharlieFoxtailsDestiny a) =
    [ mkAbility a 1 $ forced $ oneOf [AgendaAdvances #when AnyAgenda, ActAdvances #when AnyAct]
    , restricted a 2 (OnSameLocation <> thisDamageAtLeast a 1)
        $ actionAbilityWithCost (ResourceCost 2)
    , restricted a 3 (OnSameLocation <> thisDamageAtMost a 3) $ FastAbility Free
    ]

thisDamageAtLeast :: StoryAttrs -> Int -> Criterion
thisDamageAtLeast a n = if a.token Damage >= n then NoRestriction else Never

thisDamageAtMost :: StoryAttrs -> Int -> Criterion
thisDamageAtMost a n = if a.token Damage >= 0 && a.token Damage <= n then NoRestriction else Never

instance RunMessage CharlieFoxtailsDestiny where
  runMessage msg s@(CharlieFoxtailsDestiny attrs) =
    runQueueT $ scenarioI18n $ scope "charlieFoxtailsDestiny" $ case msg of
      -- The card prints both choices, so it stays on screen to be chosen from.
      ResolveThisStory iid (is attrs -> True) -> do
        focusCards [toCard attrs] $ lift $ chooseOneM iid do
          labeled "healCharlie" $ doStep 1 msg
          labeled "turnYourBackOnHim" $ doStep 2 msg
        pure s
      {- "Choice 1: Put 5 damage on it and attach it to the hut." A story sits on a
      location at AtLocation; AttachedToLocation is a placement the board does not
      lay out, so the card would be on the table but nowhere to be seen. -}
      DoStep 1 (ResolveThisStory _ (is attrs -> True)) -> do
        placeTokens attrs attrs Damage 5
        mlid <- selectOne $ locationIs Locations.hiddenHut
        pure $ CharlieFoxtailsDestiny $ attrs & placementL %~ \placement ->
          maybe placement AtLocation mlid
      DoStep 2 (ResolveThisStory iid (is attrs -> True)) -> do
        record TheSarceeAreHuntingYouDown
        tomahawk <- getSetAsideCard Assets.tomahawk
        investigators <- select $ InvestigatorAt (locationIs Locations.hiddenHut)
        unless (null investigators) do
          -- The Lita Chantler shape: the card beside the question, a portrait per
          -- investigator, and a decline.
          chooseOneM iid do
            questionLabeled "takeTheTomahawk"
            questionLabeledCard Assets.tomahawk
            portraits investigators (`takeControlOfSetAsideAsset` tomahawk)
            labeled "leaveTheTomahawk" nothing
        removeStory attrs
        pure s
      UseThisAbility _ (isSource attrs -> True) 1 -> do
        record TheSarceeAreHuntingYouDown
        removeStory attrs
        pure s
      UseThisAbility _ (isSource attrs -> True) 2 -> do
        removeTokens (attrs.ability 2) attrs Damage 1
        pure s
      UseThisAbility iid (isSource attrs -> True) 3 -> do
        -- The story is the back's own card, so it leaves before the back arrives.
        removeStory attrs
        charlie <- createAssetAt Assets.charlieFoxtail (InPlayArea iid)
        placeTokens (attrs.ability 3) charlie Damage (attrs.token Damage)
        record YouSavedCharlie
        pure s
      _ -> CharlieFoxtailsDestiny <$> liftRunMessage msg attrs
