module Arkham.Investigator.Cards.TrishScarborough (
  trishScarborough,
  TrishScarborough (..),
)
where

import Arkham.Action qualified as Action
import Arkham.Discover
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner hiding (DiscoverClues)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TrishScarborough = TrishScarborough InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock (Data)

trishScarborough :: InvestigatorCard TrishScarborough
trishScarborough =
  investigator
    TrishScarborough
    Cards.trishScarborough
    Stats
      { health = 8
      , sanity = 6
      , willpower = 2
      , intellect = 4
      , combat = 2
      , agility = 4
      }

instance HasAbilities TrishScarborough where
  getAbilities (TrishScarborough attrs) =
    [ playerLimit PerRound
        $ restrictedAbility
          attrs
          1
          ( Self
              <> oneOf
                [exists (LocationBeingDiscovered <> LocationWithAnyClues), exists $ CanEvadeEnemy (toSource attrs)]
          )
        $ freeReaction
        $ DiscoverClues #after You (LocationWithEnemy AnyEnemy) (atLeast 1)
    ]

instance HasChaosTokenValue TrishScarborough where
  getChaosTokenValue iid ElderSign (TrishScarborough attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

toLocation :: [Window] -> LocationId
toLocation [] = error "missing window"
toLocation ((windowType -> Window.DiscoverClues _ lid _ _) : _) = lid
toLocation (_ : xs) = toLocation xs

instance RunMessage TrishScarborough where
  runMessage msg i@(TrishScarborough attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (toLocation -> lid) _ -> do
      player <- getPlayer iid
      enemies <- select $ enemyAt lid <> CanEvadeEnemy (attrs.ability 1)
      push
        $ chooseOrRunOne player
        $ [Label "Discover 1 additional clue at that location" [Msg.DiscoverClues iid $ discover lid attrs 1]]
        <> [ Label
            "Automatically evade that enemy"
            [chooseOrRunOne player [targetLabel enemy [EnemyEvaded iid enemy] | enemy <- enemies]]
           | notNull enemies
           ]
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      mAction <- getSkillTestAction
      case mAction of
        Just Action.Investigate -> do
          mLocation <- getMaybeLocation iid
          locations <- select $ RevealedLocation <> maybe Anywhere (not_ . LocationWithId) mLocation
          when (notNull locations) do
            player <- getPlayer iid
            push $ chooseOne player $ Label "Do not choose a different location" []
              : [ targetLabel
                  location
                  [ SetSkillTestTarget (toTarget location)
                  , skillTestModifier attrs iid (AsIfAt location)
                  ]
                | location <- locations
                ]
        _ -> pure ()
      pure i
    _ -> TrishScarborough <$> runMessage msg attrs
