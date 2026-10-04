module Arkham.Homebrew.AgainstTheWendigo.Stories.CharlieFoxtailsDestiny (
  charlieFoxtailsDestiny,
) where

import Arkham.Ability
import Arkham.Card (toCard)
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

{- | "Choose if you're going to heal Charlie's wounds despite his abandonment of
the previous expedition (Choice 1) or if you turn your back on him because you
do not trust him (Choice 2)."

Choice 1 is a race: the card goes onto the Hidden Hut with 5 damage, costs 2
resources a point to heal, and is lost -- taking the Sarcee with it -- the
moment an act or agenda advances. Get it to 3 damage or less and Charlie comes
with you instead. Choice 2 skips the race and leaves his tomahawk behind.
-}
charlieFoxtailsDestiny :: StoryCard CharlieFoxtailsDestiny
charlieFoxtailsDestiny = persistStory $ story CharlieFoxtailsDestiny Cards.charlieFoxtailsDestiny

instance HasAbilities CharlieFoxtailsDestiny where
  getAbilities (CharlieFoxtailsDestiny a) =
    [ -- "Forced - When you advance an agenda or an act, remove Charlie
      -- Foxtail's Destiny from the game, and record that the Sarcee are hunting
      -- you down."
      mkAbility a 1 $ forced $ oneOf [AgendaAdvances #when AnyAgenda, ActAdvances #when AnyAct]
    , -- "{action} Spend 2 resources: Heal 1 damage on Charlie Foxtail's Destiny."
      restricted a 2 (OnSameLocation <> thisDamageAtLeast a 1)
        $ actionAbilityWithCost (ResourceCost 2)
    , -- "{fast}: Flip Charlie Foxtail's Destiny over... An investigator on this
      -- location takes control of Charlie Foxtail."
      restricted a 3 (OnSameLocation <> thisDamageAtMost a 3) $ FastAbility Free
    ]

{- | Charlie's wounds read back as criteria. There is no story matcher for a
story's own tokens, so both are expressed against what the card knows.
-}
thisDamageAtLeast :: StoryAttrs -> Int -> Criterion
thisDamageAtLeast a n = if a.token Damage >= n then NoRestriction else Never

thisDamageAtMost :: StoryAttrs -> Int -> Criterion
thisDamageAtMost a n = if a.token Damage > 0 && a.token Damage <= n then NoRestriction else Never

instance RunMessage CharlieFoxtailsDestiny where
  runMessage msg s@(CharlieFoxtailsDestiny attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      chooseOneM iid $ scenarioI18n $ scope "charlieFoxtailsDestiny" do
        labeled "healCharlie" do
          selectForMaybeM (locationIs Locations.hiddenHut) \lid ->
            push $ StoryMessage $ PlaceStory (toCard attrs) (AttachedToLocation lid)
          placeTokens attrs attrs Damage 5
        labeled "turnYourBackOnHim" do
          record TheSarceeAreHuntingYouDown
          selectForMaybeM (locationIs Locations.hiddenHut) \lid ->
            createAssetAt_ Assets.tomahawk (AtLocation lid)
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
      charlie <- createAssetAt Assets.charlieFoxtail (InPlayArea iid)
      -- "Put the remaining damage on the other side."
      placeTokens (attrs.ability 3) charlie Damage (attrs.token Damage)
      record YouSavedCharlie
      removeStory attrs
      pure s
    _ -> CharlieFoxtailsDestiny <$> liftRunMessage msg attrs
