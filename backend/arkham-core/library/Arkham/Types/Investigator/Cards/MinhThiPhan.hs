module Arkham.Types.Investigator.Cards.MinhThiPhan
  ( minhThiPhan
  , MinhThiPhan(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window

newtype MinhThiPhan = MinhThiPhan InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env MinhThiPhan where
  getModifiersFor source target (MinhThiPhan attrs) =
    getModifiersFor source target attrs

minhThiPhan :: MinhThiPhan
minhThiPhan = MinhThiPhan $ baseAttrs
  "03002"
  "Minh Thi Phan"
  Seeker
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 4
    , combat = 2
    , agility = 2
    }
  [Assistant]

ability :: InvestigatorAttrs -> InvestigatorId -> Card -> Ability
ability attrs iid card =
  mkAbility attrs 1 (ReactionAbility Free)
    & (abilityLimitL .~ PerInvestigatorLimit iid PerRound 1)
    & (abilityMetadataL ?~ TargetMetadata (CardIdTarget $ toCardId card))

instance InvestigatorRunner env => HasAbilities env MinhThiPhan where
  getAbilities i (AfterCommitedCard who card) (MinhThiPhan attrs) = do
    atSameLocation <- liftA2 (==) (getId @LocationId i) (getId @LocationId who)
    pure [ ability attrs who card | atSameLocation ]
  getAbilities i window (MinhThiPhan attrs) = getAbilities i window attrs

instance HasTokenValue env MinhThiPhan where
  getTokenValue (MinhThiPhan attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue (MinhThiPhan attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env MinhThiPhan where
  runMessage msg i@(MinhThiPhan attrs) = case msg of
    UseCardAbility _ source (Just (TargetMetadata target)) 1 _
      | isSource attrs source -> i <$ push
        (CreateEffect
          (unInvestigatorId $ toId attrs)
          Nothing
          (toSource attrs)
          target
        )
    ResolveToken _ ElderSign iid | iid == toId attrs -> do
      investigatorIds <- getInvestigatorIds
      skills <-
        concat
          <$> traverse
                (fmap (map unCommitedSkillId) . getSetList @CommittedSkillId)
                investigatorIds
      i <$ when
        (notNull skills)
        (push
          (chooseOne
            iid
            [ TargetLabel
                (SkillTarget skill)
                [ CreateEffect
                    (unInvestigatorId $ toId attrs)
                    Nothing
                    (TokenEffectSource ElderSign)
                    (SkillTarget skill)
                ]
            | skill <- skills
            ]
          )
        )
    _ -> MinhThiPhan <$> runMessage msg attrs
