module Arkham.Treachery.Cards.NoTurningBack
  ( noTurningBack
  , NoTurningBack(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype NoTurningBack = NoTurningBack TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noTurningBack :: TreacheryCard NoTurningBack
noTurningBack = treachery NoTurningBack Cards.noTurningBack

instance HasModifiersFor NoTurningBack where
  getModifiersFor (InvestigatorTarget iid) (NoTurningBack attrs) =
    case treacheryPlacement attrs of
      TreacheryAttachedTo (LocationTarget lid) -> do
        onNoTurningBack <- iid <=~> investigatorAt lid
        pure $ toModifiers
          attrs
          [if onNoTurningBack then CannotMove else CannotEnter lid]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities NoTurningBack where
  getAbilities (NoTurningBack a) =
    [ restrictedAbility
          a
          1
          (OnLocation $ LocationMatchAny
            [ LocationWithTreachery (TreacheryWithId $ toId a)
            , ConnectedTo (LocationWithTreachery (TreacheryWithId $ toId a))
            ]
          )
        $ ActionAbility Nothing
        $ ActionCost 1
    ]

instance RunMessage NoTurningBack where
  runMessage msg t@(NoTurningBack attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      targets <-
        selectList
        $ LocationMatchAny
            [ locationWithInvestigator iid
            , ConnectedFrom (locationWithInvestigator iid)
            ]
        <> LocationWithoutTreachery (treacheryIs Cards.noTurningBack)
      unless (null targets) $ do
        push $ chooseOrRunOne
          iid
          [ targetLabel x [AttachTreachery (toId attrs) (LocationTarget x)]
          | x <- targets
          ]
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      hasPickaxe <- getHasSupply iid Pickaxe
      push
        $ chooseOrRunOne iid
        $ Label
            "Test {combat} (3)"
            [ BeginSkillTest
                iid
                (toSource attrs)
                (toTarget attrs)
                Nothing
                SkillCombat
                3
            ]
        : [ Label "Check your supplies" [Discard (toTarget attrs)]
          | hasPickaxe
          ]
      pure t
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ Discard (toTarget attrs)
        pure t
    _ -> NoTurningBack <$> runMessage msg attrs
