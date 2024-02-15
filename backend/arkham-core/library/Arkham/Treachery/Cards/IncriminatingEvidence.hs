module Arkham.Treachery.Cards.IncriminatingEvidence (
  incriminatingEvidence,
  IncriminatingEvidence (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait (Trait (CrimeScene))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype IncriminatingEvidence = IncriminatingEvidence TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

incriminatingEvidence :: TreacheryCard IncriminatingEvidence
incriminatingEvidence = treachery IncriminatingEvidence Cards.incriminatingEvidence

instance HasModifiersFor IncriminatingEvidence where
  getModifiersFor target (IncriminatingEvidence a)
    | target `elem` treacheryAttachedTarget a =
        pure $ toModifiers a [AddTrait CrimeScene, ShroudModifier 2]
  getModifiersFor _ _ = pure []

instance HasAbilities IncriminatingEvidence where
  getAbilities (IncriminatingEvidence attrs) = case treacheryAttachedTarget attrs of
    Just (LocationTarget lid) ->
      [ mkAbility attrs 1
          $ freeReaction
          $ SkillTestResult #when You (WhileInvestigating $ LocationWithId lid)
          $ SuccessResult AnyValue
      ]
    _ -> []

instance RunMessage IncriminatingEvidence where
  runMessage msg t@(IncriminatingEvidence attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      nonCrimeScenes <- select $ NearestLocationToYou $ NotLocation $ LocationWithTrait CrimeScene
      player <- getPlayer iid
      pushIfAny nonCrimeScenes
        $ chooseOrRunOne player
        $ targetLabels nonCrimeScenes
        $ only
        . attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      case treacheryAttachedTarget attrs of
        Just (LocationTarget lid) -> do
          push
            $ skillTestModifier (toAbilitySource attrs 1) lid (AlternateSuccessfullInvestigation $ toTarget attrs)
        _ -> error "Unexpected"
      pure t
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> IncriminatingEvidence <$> runMessage msg attrs
