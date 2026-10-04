module Arkham.Homebrew.AgainstTheWendigo.Locations.TheHeartOfTheForest (
  theHeartOfTheForest,
) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move (forcedMoveTo)

newtype TheHeartOfTheForest = TheHeartOfTheForest LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeartOfTheForest :: LocationCard TheHeartOfTheForest
theHeartOfTheForest = location TheHeartOfTheForest Locations.theHeartOfTheForest 4 (Static 0)

{- | "You cannot leave this location, unless an investigator in The Heart of the
Forest succeeds at an Investigate action in the same round."

The way out is held open for the rest of the round by a flag in the location's
own meta, set by that successful investigation and cleared when the round ends.
-}
instance HasModifiersFor TheHeartOfTheForest where
  getModifiersFor (TheHeartOfTheForest a) =
    modifySelect a (InvestigatorAt $ be a) [CannotMove | not (wayOutIsOpen a)]

wayOutIsOpen :: LocationAttrs -> Bool
wayOutIsOpen a = toResultDefault False a.meta

instance HasAbilities TheHeartOfTheForest where
  getAbilities (TheHeartOfTheForest a) =
    extendRevealed
      a
      [ {- | "{reaction} When an investigator performs Investigate in The Heart
        of the Forest, lose 1 action on your next turn: The Heart of the Forest
        gets -1 shroud for this investigation." -}
        restricted a 1 Here
          $ triggered
            (InitiatedSkillTest #when You AnySkillType AnySkillTestValue (WhileInvestigating (be a)))
            mempty
      , mkAbility a 2 $ forced $ RoundEnds #when
      ]

instance RunMessage TheHeartOfTheForest where
  runMessage msg l@(TheHeartOfTheForest attrs) = runQueueT $ case msg of
    {- | "Put The Heart of the Forest into play. Move each investigator from the
    Impenetrable Forest to this location. This move does not trigger attacks of
    opportunity." -}
    Revelation _ (isSource attrs -> True) -> do
      selectEach (InvestigatorAt $ locationIs Locations.impenetrableForest) \iid ->
        forcedMoveTo attrs iid attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      nextTurnModifier iid (attrs.ability 1) iid (FewerActions 1)
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) attrs.id (ShroudModifier (-1))
      pure l
    -- The way out closes again at the end of the round.
    UseThisAbility _ (isSource attrs -> True) 2 ->
      pure $ TheHeartOfTheForest $ setMeta False attrs
    Successful (Action.Investigate, _) _ _ (isTarget attrs -> True) _ ->
      pure $ TheHeartOfTheForest $ setMeta True attrs
    _ -> TheHeartOfTheForest <$> liftRunMessage msg attrs
