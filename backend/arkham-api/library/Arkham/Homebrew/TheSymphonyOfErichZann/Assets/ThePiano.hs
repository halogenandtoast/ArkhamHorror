module Arkham.Homebrew.TheSymphonyOfErichZann.Assets.ThePiano (thePiano) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelf)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits qualified as T
import Arkham.Matcher

newtype ThePiano = ThePiano AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePiano :: AssetCard ThePiano
thePiano = asset ThePiano Cards.thePiano

instance HasModifiersFor ThePiano where
  getModifiersFor (ThePiano a) = modifySelf a []

instance HasAbilities ThePiano where
  {- "[reaction]: If you are Isabel La Fratta, after you perform 4 actions of
  different types during your turn at this location: Parley."

  A reaction, not an action: the fourth action is what opens the window, and by
  then there are none left to spend. `handleTakenActions` raises the streak
  window off `longestUniqueStreak`, the same one Captivating Performance (3)
  uses for three. -}
  getAbilities (ThePiano a) =
    [ restricted
        a
        1
        ( exists (TreacheryWithTrait T.Piano <> InPlayTreachery)
            <> youExist
              (InvestigatorWithTitle "Isabel La Fratta" <> at_ (locationWithAsset a.id))
        )
        $ triggeredAction #parley (PerformedDifferentTypesOfActionsInARow #after You 4 AnyAction) Free
    ]

instance RunMessage ThePiano where
  runMessage msg a@(ThePiano attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      card <- fetchCard Stories.thePianosMuse
      readStory iid card Stories.thePianosMuse
      pure a
    _ -> ThePiano <$> liftRunMessage msg attrs
