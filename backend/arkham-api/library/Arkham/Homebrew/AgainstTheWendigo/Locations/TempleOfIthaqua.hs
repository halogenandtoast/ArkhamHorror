module Arkham.Homebrew.AgainstTheWendigo.Locations.TempleOfIthaqua (templeOfIthaqua) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectWhen, modifySelfWhen)
import Arkham.Helpers.SkillTest (getSkillTestAction)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Stories
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (Horror))

newtype TempleOfIthaqua = TempleOfIthaqua LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfIthaqua :: LocationCard TempleOfIthaqua
templeOfIthaqua = location TempleOfIthaqua Cards.templeOfIthaqua 3 (Static 0)

{- | The Knowledge of the Cold attaches to the Temple and only then grants it the
horror abilities below, so everything the story adds is gated on the story being
in play.
-}
knowledgeIsBeingRead :: Criterion
knowledgeIsBeingRead = exists (StoryIs $ Stories.theKnowledgeOfTheCold.cardCode)

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
    extendRevealed
      a
      [ -- "{reaction} If there are no more clues on Temple of Ithaqua: Read the
        -- first part of The Knowledge of the Cold story card."
        restricted a 1 (Here <> notExists (be a <> LocationWithAnyClues) <> not_ knowledgeIsBeingRead)
          $ triggered (DiscoverClues #after Anyone (be a) AnyValue) mempty
      , {- | Granted by The Knowledge of the Cold: "{action} Put 1 horror on this
        location: Test [willpower] (2+X), where X is the number of horror on this
        location. Take 1 horror for each point you fail by." -}
        restricted a 2 (Here <> knowledgeIsBeingRead) actionAbility
      ]

instance RunMessage TempleOfIthaqua where
  runMessage msg l@(TempleOfIthaqua attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs Stories.theKnowledgeOfTheCold
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
