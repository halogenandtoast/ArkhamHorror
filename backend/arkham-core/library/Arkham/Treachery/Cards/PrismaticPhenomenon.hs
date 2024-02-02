module Arkham.Treachery.Cards.PrismaticPhenomenon (
  prismaticPhenomenon,
  PrismaticPhenomenon (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PrismaticPhenomenon = PrismaticPhenomenon TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

prismaticPhenomenon :: TreacheryCard PrismaticPhenomenon
prismaticPhenomenon = treachery PrismaticPhenomenon Cards.prismaticPhenomenon

instance HasModifiersFor PrismaticPhenomenon where
  getModifiersFor (InvestigatorTarget iid) (PrismaticPhenomenon attrs) =
    pure
      $ toModifiers attrs
      $ [ ActionCostOf (FirstOneOfPerformed [#draw, #resource, #play]) 1
        | treacheryOnInvestigator iid attrs
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities PrismaticPhenomenon where
  getAbilities (PrismaticPhenomenon a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ SkillTestResult #when You (WhileInvestigating Anywhere)
        $ SuccessResult AnyValue
    ]

instance RunMessage PrismaticPhenomenon where
  runMessage msg t@(PrismaticPhenomenon attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mTarget <- getSkillTestTarget
      case mTarget of
        Nothing -> error "invalid target"
        Just target -> do
          push
            $ skillTestModifier
              (toAbilitySource attrs 1)
              target
              (AlternateSuccessfullInvestigation $ toTarget attrs)
      pure t
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> PrismaticPhenomenon <$> runMessage msg attrs
