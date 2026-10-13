module Arkham.Homebrew.AgainstTheWendigo.Locations.TempleOfIthaqua (templeOfIthaqua) where

import Arkham.Ability
import Arkham.Ability.Types qualified as AT
import Arkham.Constants (pattern AbilityInvestigate)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectWhen, modifySelfWhen)
import Arkham.Helpers.SkillTest (getSkillTestAction)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgainstTheWendigo.Helpers (
  connectsEastOrWest,
  mountainRangeReveal,
  spotlightStory,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (Horror))

newtype TempleOfIthaqua = TempleOfIthaqua LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfIthaqua :: LocationCard TempleOfIthaqua
templeOfIthaqua = locationWith TempleOfIthaqua Cards.templeOfIthaqua 3 (Static 0) connectsEastOrWest

{- | The Knowledge of the Cold attaches to the Temple and only then grants it the
horror abilities below, so everything the story adds is gated on the story being
in play.
-}
knowledgeIsBeingRead :: Criterion
knowledgeIsBeingRead = exists (StoryIs Stories.theKnowledgeOfTheCold.cardCode)

instance HasModifiersFor TempleOfIthaqua where
  getModifiersFor (TempleOfIthaqua a) = do
    -- The unrevealed Mountain Range prints "You cannot move into the Mountain Range."
    blocked <- modifySelect a Anyone [CannotEnter a.id | not a.revealed]
    -- "Use [willpower] instead of [intellect] when you Investigate in this location."
    investigating <- (== Just #investigate) <$> getSkillTestAction
    willpowerForIntellect <-
      modifySelectWhen
        a
        (a.revealed && investigating)
        (InvestigatorAt $ be a)
        [UseSkillInsteadOf #intellect #willpower]
    -- "This location gains +1 Victory for each horror on it."
    let horror = a.token Horror
    victory <- modifySelfWhen a (horror > 0) [GainVictory horror]
    pure $ blocked <> willpowerForIntellect <> victory

instance HasAbilities TempleOfIthaqua where
  getAbilities (TempleOfIthaqua a) =
    [mountainRangeReveal a | not a.revealed]
      <> map
        useWillpower
        ( extendRevealed
            a
            [ {- "{reaction} If there are no more clues on Temple of Ithaqua: Read
              the first part of The Knowledge of the Cold story card."

              An anytime reaction rather than one hanging off the discover
              window: the condition is "there are no more clues", not "you just
              took the last one", so clearing the location by any route -- or
              walking back to it later -- has to still offer it. See
              'fateReaction', which reads the same way. -}
              onlyOnce
                $ restricted
                  a
                  1
                  (Here <> thisExists a LocationWithoutClues <> not_ knowledgeIsBeingRead)
                $ freeReaction AnyWindow
            , {- Granted by The Knowledge of the Cold: "{action} Put 1 horror on
              this location: Test [willpower] (2+X), where X is the number of
              horror on this location. Take 1 horror for each point you fail by." -}
              restricted a 2 (Here <> knowledgeIsBeingRead) actionAbility
            , {- Also granted by it: "{reaction} If this location has 3 horrors on
              it, an investigator on this location reads the second part of The
              Knowledge of the Cold." Anytime while that holds, like the no-clue
              reads -- the condition is a state, not a moment. -}
              onlyOnce
                $ restricted
                  a
                  3
                  (Here <> knowledgeIsBeingRead <> thisExists a (LocationWithHorror $ atLeast 3))
                $ freeReaction AnyWindow
            ]
        )
   where
    {- "Use [willpower] instead of [intellect] when you Investigate in this
    location." The swap itself is a modifier (see 'HasModifiersFor'), which the
    test honours but the button cannot see: a basic ability's skill is whatever
    the ability declares, so the declared skill is changed here as well. -}
    useWillpower :: Ability -> Ability
    useWillpower ab
      | a.revealed
      , ab.index == AbilityInvestigate =
          ab {AT.abilityType = investigateActionWith #willpower mempty}
      | otherwise = ab

instance RunMessage TempleOfIthaqua where
  runMessage msg l@(TempleOfIthaqua attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 98 -> do
      reveal attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      card <- fetchCard Stories.theKnowledgeOfTheCold
      {- The CARD as the target, not this location: a story read against a board
      entity visually replaces it, while one with no board target lands
      'Unplaced' and gets the card spotlight. -}
      readStory iid card Stories.theKnowledgeOfTheCold
      -- Reading it takes it out of the set-aside pile; see 'readStoryAndChoose'.
      obtainCard card
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      selectEach (storyIs Stories.theKnowledgeOfTheCold) (spotlightStory iid)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      placeTokens (attrs.ability 2) attrs Horror 1
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed $ 2 + attrs.token Horror + 1)
      pure l
    FailedSkillTest iid _ (isAbilitySource attrs 2 -> True) SkillTestInitiatorTarget {} _ n -> do
      assignHorror iid (attrs.ability 2) n
      pure l
    _ -> TempleOfIthaqua <$> liftRunMessage msg attrs
