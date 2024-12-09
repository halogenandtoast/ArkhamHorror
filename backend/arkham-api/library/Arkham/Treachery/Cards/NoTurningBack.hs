module Arkham.Treachery.Cards.NoTurningBack (
  noTurningBack,
  NoTurningBack (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype NoTurningBack = NoTurningBack TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noTurningBack :: TreacheryCard NoTurningBack
noTurningBack = treachery NoTurningBack Cards.noTurningBack

instance HasModifiersFor NoTurningBack where
  getModifiersFor (NoTurningBack attrs) = case attrs.placement.attachedTo of
    Just (LocationTarget lid) -> do
      modifySelectMaybe attrs Anyone \iid -> do
        onNoTurningBack <- iid <=~> investigatorAt lid
        pure [if onNoTurningBack then CannotMove else CannotEnter lid]
    _ -> pure mempty

instance HasAbilities NoTurningBack where
  getAbilities (NoTurningBack a) =
    [ skillTestAbility
        $ restrictedAbility
          a
          1
          ( OnLocation
              $ LocationMatchAny
                [ LocationWithTreachery (TreacheryWithId $ toId a)
                , ConnectedTo (LocationWithTreachery (TreacheryWithId $ toId a))
                ]
          )
        $ ActionAbility []
        $ ActionCost 1
    ]

instance RunMessage NoTurningBack where
  runMessage msg t@(NoTurningBack attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      targets <-
        select
          $ LocationMatchAny
            [ locationWithInvestigator iid
            , ConnectedFrom (locationWithInvestigator iid)
            ]
          <> LocationWithoutTreachery (treacheryIs Cards.noTurningBack)
      player <- getPlayer iid
      unless (null targets) $ do
        push $ chooseOrRunOne player [targetLabel x [attachTreachery attrs x] | x <- targets]
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      hasPickaxe <- getHasSupply iid Pickaxe
      player <- getPlayer iid
      sid <- getRandom
      push
        $ chooseOrRunOne player
        $ Label "Test {combat} (3)" [beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 3)]
        : [Label "Check your supplies" [toDiscardBy iid (attrs.ability 1) attrs] | hasPickaxe]
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> NoTurningBack <$> runMessage msg attrs
