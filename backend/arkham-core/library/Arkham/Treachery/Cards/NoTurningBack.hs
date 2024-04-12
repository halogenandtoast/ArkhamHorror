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
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype NoTurningBack = NoTurningBack TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noTurningBack :: TreacheryCard NoTurningBack
noTurningBack = treachery NoTurningBack Cards.noTurningBack

instance HasModifiersFor NoTurningBack where
  getModifiersFor (InvestigatorTarget iid) (NoTurningBack attrs) =
    case treacheryPlacement attrs of
      TreacheryAttachedTo (LocationTarget lid) -> do
        onNoTurningBack <- iid <=~> investigatorAt lid
        pure
          $ toModifiers
            attrs
            [if onNoTurningBack then CannotMove else CannotEnter lid]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities NoTurningBack where
  getAbilities (NoTurningBack a) =
    [ restrictedAbility
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
        push
          $ chooseOrRunOne
            player
            [ targetLabel x [AttachTreachery (toId attrs) (LocationTarget x)]
            | x <- targets
            ]
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      hasPickaxe <- getHasSupply iid Pickaxe
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label
          "Test {combat} (3)"
          [beginSkillTest iid (attrs.ability 1) (toTarget attrs) SkillCombat (Fixed 3)]
        : [ Label "Check your supplies" [toDiscardBy iid (toAbilitySource attrs 1) attrs]
          | hasPickaxe
          ]
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> NoTurningBack <$> runMessage msg attrs
