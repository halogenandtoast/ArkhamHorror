module Arkham.Homebrew.AgainstTheWendigo.Locations.Ithaqua (ithaqua) where

import Arkham.Ability
import Arkham.Action (Action)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgainstTheWendigo.Actions (pattern Navigate, pattern WalkAlongTheRiver)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (forcedMoveTo)

newtype Ithaqua = Ithaqua LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ithaqua :: LocationCard Ithaqua
ithaqua = location Ithaqua Cards.ithaqua 4 (Static 0)

{- | "An investigator on this location can only perform Fight, Evade, or Flee
Ithaqua actions."

Flee Ithaqua is ability 1 below and carries no action designator, so banning the
designated actions one by one leaves it -- and Fight and Evade -- reachable.
Activate is left alone for the same reason: an undesignated ability action is
how Flee Ithaqua is taken.
-}
forbiddenActions :: [Action]
forbiddenActions =
  [ #draw
  , #engage
  , #investigate
  , #move
  , #parley
  , #play
  , #resign
  , #resource
  , #explore
  , #circle
  , Navigate
  , WalkAlongTheRiver
  ]

instance HasModifiersFor Ithaqua where
  getModifiersFor (Ithaqua a) =
    modifySelect
      a
      (InvestigatorAt $ be a)
      [CannotTakeAction $ AnyActionTarget $ map IsAction forbiddenActions]

instance HasAbilities Ithaqua where
  getAbilities (Ithaqua a) =
    extendRevealed
      a
      [ -- "{action}: Flee Ithaqua."
        restricted a 1 Here actionAbility
      , -- "Forced - At the end of each round: Each investigator in this location
        -- takes 1 direct horror."
        restricted a 2 (exists $ InvestigatorAt $ be a) $ forced $ RoundEnds #when
      , -- "If an investigator is defeated by horror on this location, she or he
        -- is driven insane."
        mkAbility a 3
          $ forced
          $ Matcher.InvestigatorDefeated #when ByHorror (InvestigatorAt $ be a)
      ]

instance RunMessage Ithaqua where
  runMessage msg l@(Ithaqua attrs) = runQueueT $ case msg of
    -- "Test [willpower] (4) or [agility] (4)."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseSkillM iid [#willpower, #agility] \sType ->
        beginSkillTest sid iid (attrs.ability 1) iid sType (Fixed 4)
      pure l
    -- "If you succeed, move to a connected location."
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      destinations <- select $ connectedFrom (be attrs)
      chooseOrRunOneM iid $ targets destinations $ forcedMoveTo (attrs.ability 1) iid
      pure l
    -- "If you fail, take 1 direct horror."
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      directHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectEach (InvestigatorAt $ be attrs) \iid -> directHorror iid (attrs.ability 2) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      push $ DrivenInsane iid
      pure l
    _ -> Ithaqua <$> liftRunMessage msg attrs
