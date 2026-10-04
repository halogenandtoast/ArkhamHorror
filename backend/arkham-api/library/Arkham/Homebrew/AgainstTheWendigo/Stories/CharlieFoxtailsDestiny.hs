module Arkham.Homebrew.AgainstTheWendigo.Stories.CharlieFoxtailsDestiny (
  charlieFoxtailsDestiny,
) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (scenarioI18n)
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype CharlieFoxtailsDestiny = CharlieFoxtailsDestiny StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlieFoxtailsDestiny :: StoryCard CharlieFoxtailsDestiny
charlieFoxtailsDestiny = story CharlieFoxtailsDestiny Cards.charlieFoxtailsDestiny

{- | "Choose if you're going to heal Charlie's wounds despite his abandonment of
the previous expedition (Choice 1) or if you turn your back on him because you
do not trust him (Choice 2)."

Choice 1 puts Charlie into play wounded, to be healed before an act or agenda
advances; choice 2 leaves the Tomahawk behind instead. Either way the Sarcee
end up hunting you unless Charlie is saved.
-}
instance RunMessage CharlieFoxtailsDestiny where
  runMessage msg s@(CharlieFoxtailsDestiny attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      chooseOneM iid $ scenarioI18n $ scope "charlieFoxtailsDestiny" do
        labeled "healCharlie" do
          charlie <- createAssetAt Assets.charlieFoxtail (InPlayArea iid)
          placeTokens attrs charlie #damage 5
          record YouSavedCharlie
        labeled "turnYourBackOnHim" do
          record TheSarceeAreHuntingYouDown
          createAssetAt_ Assets.tomahawk (InPlayArea iid)
      removeStory attrs
      pure s
    _ -> CharlieFoxtailsDestiny <$> liftRunMessage msg attrs
