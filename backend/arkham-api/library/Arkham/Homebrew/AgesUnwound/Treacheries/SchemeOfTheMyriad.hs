module Arkham.Homebrew.AgesUnwound.Treacheries.SchemeOfTheMyriad (schemeOfTheMyriad) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest.Lifted (investigateEdit_)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Homebrew.AgesUnwound.Traits
import Arkham.Keyword (Keyword (Aloof, Hunter))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype SchemeOfTheMyriad = SchemeOfTheMyriad TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving anyclass IsTreachery

schemeOfTheMyriad :: TreacheryCard SchemeOfTheMyriad
schemeOfTheMyriad = treachery SchemeOfTheMyriad Cards.schemeOfTheMyriad

-- | Attached to Rome; nothing else in the set attaches it elsewhere.
hostLocation :: TreacheryAttrs -> LocationMatcher
hostLocation a = case a.placement of
  AttachedToLocation lid -> LocationWithId lid
  AtLocation lid -> LocationWithId lid
  _ -> Nowhere

-- | "[[Myriad]] enemies at attached location lose aloof and hunter."
instance HasModifiersFor SchemeOfTheMyriad where
  getModifiersFor (SchemeOfTheMyriad a) =
    modifySelect
      a
      (EnemyAt (hostLocation a) <> withTrait Myriad)
      [RemoveKeyword Aloof, RemoveKeyword Hunter]

{- | "[action] If there are no ready [[Myriad]] enemies at this location:
__Investigate.__ If you succeed, instead of discovering clues, flip this card
over and resolve its text."
-}
instance HasAbilities SchemeOfTheMyriad where
  getAbilities (SchemeOfTheMyriad a) =
    [ campaignI18n
        $ withI18nTooltip "schemeOfTheMyriad.investigate"
        $ withI18nResultLabel "schemeOfTheMyriad.investigate"
        $ investigateAbility a 1 mempty
        $ OnSameLocation
        <> notExists (EnemyAt (hostLocation a) <> withTrait Myriad <> ReadyEnemy)
    ]

instance RunMessage SchemeOfTheMyriad where
  runMessage msg t@(SchemeOfTheMyriad attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigateEdit_ sid iid (attrs.ability 1) (setTarget attrs)
      pure t
    {- Returning without the base discovery is how "instead of discovering clues"
    is spelled on a card that owns the Investigate itself -- see Grand Bazaar
    (Jeweler's Road). -}
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) (isTarget attrs -> True) _ -> do
      readStory iid attrs Stories.harnessingATearInReality
      pure t
    _ -> SchemeOfTheMyriad <$> liftRunMessage msg attrs
